

open Elements

type ('i, 'o, 'v) stream =
  | Close
  | Value of 'v
  | Yield of ('o  * ('i, 'o, 'v) stream)
  | Await of ('i -> ('i, 'o, 'v) stream)

let close = Close

(* let return x = *)
  (* Yield (x, close) *)

let return x =
  Value x

let rec cat =
  Await (fun i -> Yield (i, cat))

(* let rec concat s1 s2 = *)
  (* match s1, s2 with *)
  (* | Close          , _              -> s2 *)
  (* | Yield (b, s1') , _              -> Yield (b, concat s1' s2) *)
  (* | Await k        , Yield (b, s2') -> concat (k b) s2' *)
  (* | Await _        , Await k        -> Await (fun a -> concat s1 (k a)) *)
  (* | Await _        , Close          -> s1 *)

let rec compose s1 s2 =
  match s1, s2 with
  | Value x        , _              -> Value x
  | Close          , _              -> Close
  | Yield (b, s1') , _              -> Yield (b, compose s1' s2)
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> Await (fun a -> compose s1 (k a))
  | Await _        , Close          -> Close
  | _              , Value x        -> Value x

let (<-<) s1 s2 = compose s1 s2
let (>->) s2 s1 = compose s1 s2

(* Original *)
(* let rec (>>=) s f = *)
  (* match s with *)
  (* | Close         -> Close *)
  (* | Yield (x, s') -> compose (f x) (s' >>= f) *)
  (* | Await k       -> Await (fun x -> k x >>= f) *)

let rec (>>=) s f =
  match s with
  | Close         -> Close
  | Value x       -> f x
  | Yield (o, s') -> Yield (o, s' >>= f)
  | Await k       -> Await (fun i -> k i >>= f)


let (>>) s1 s2 =
  s1 >>= fun () -> s2

let yield b =
  Yield (b, Close)

let await =
  Await (fun a -> Yield (a, Close))

let rec run s =
  match s with
  | Close            -> None
  | Value x          -> Some x
  | Yield (x, s')    -> run s'
  | Await k          -> run (k ())

let nth n =
  let rec loop i =
    Await (fun a ->
        if i = n
        then Value a
        else loop (i + 1))
  in loop 0

let rec take n =
  if n = 0
  then Close
  else Await (fun i -> Yield (i, take (n - 1)))

let rec map f =
  Await (fun a -> Yield (f a, map f))

let rec print =
  Await (fun a -> print_endline a; print)

let rec of_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> Yield (line, of_chan ch)
  | None      -> Close

let rec of_list xs =
  match xs with
  | x::xs' -> Yield (x, of_list xs')
  | []     -> Close

let to_list producer =
  let rec go acc s =
    match s with
    | Close         -> Value acc
    | Value _       -> Value acc
    | Yield (o, s') -> go (o::acc) s'
    | Await k       -> failwith "should be closed"
  in go [] producer

(* let rec mux t = *)
  (* match t with *)
  (* | Yield (o, t') -> (Yield (o, t'), Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | Yield (o1, t1'), Yield (o2, t2') -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



