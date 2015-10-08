

open Elements

type 'a input =
  | Next of 'a
  | Stop

type ('i, 'o, 'a) stream =
  | Close
  | Error of exn
  | Value of 'a
  | Yield of ('o  * ('i, 'o, 'a) stream)
  | Await of ('i -> ('i, 'o, 'a) stream)

let return x =
  Value x

let close = Close

let rec cat =
  Await (fun i -> Yield (i, cat))

let rec compose s1 s2 =
  match s1, s2 with
  | _              , Error e        -> Error e
  | Error e        , _              -> Error e
  | Value x        , _              -> Value x
  | Close          , _              -> Close
  | Yield (b, s1') , _              -> Yield (b, compose s1' s2)
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> Await (fun a -> compose s1 (k a))
  | Await _        , Close          -> Close
  | Await _        , Value x        -> Value x

let (<=) s1 s2 = compose s1 s2
let (=>) s2 s1 = compose s1 s2

let rec (>>=) s f =
  match s with
  | Error e       -> Error e
  | Value x       -> Value x
  | Close         -> f ()
  | Yield (o, s') -> Yield (o, s' >>= f)
  | Await k       -> Await (fun i -> k i >>= f)

let (>>) s1 s2 =
  s1 >>= fun () -> s2

let yield b =
  Yield (b, close)

let await =
  Await (fun a -> Yield (a, close))

let rec run s =
  match s with
  | Value r          -> r
  | Error e          -> raise e
  | Close            -> fail "closed stream"
  | Await k          -> run (k Void)
  | Yield (Void, s') -> run s'

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

let get_line () = of_chan stdin

let rec of_list xs =
  match xs with
  | x::xs' -> Yield (x, of_list xs')
  | []     -> Close

let rec repeat x =
  Yield (x, repeat x)

let rec yes = Yield ("y", yes)

let rec count n =
  Yield (n, count (n + 1))

(* `nth n` - yield the result    *)
(* let nth n =                   *)
(*   let rec loop i =            *)
(*     Await (fun a ->           *)
(*         if i = n              *)
(*         then Yield (a, empty) *)
(*         else loop (i + 1))    *)
(*   in loop 0                   *)

(* `nth n` - returns the result  *)
let nth n : ('a, void, 'a) stream =
  let rec loop i =
    Await (fun x ->
        if i = n
        then Value x
        else loop (i + 1))
  in loop 0

let to_list : ('a option, void, 'a list) stream =
  let rec loop acc =
    Await (function
        | None   -> return acc
        | Some x -> loop (x::acc))
  in loop []

let head =
  Await (function Next a -> Value a | Stop -> Error End_of_file)

let length () =
  let rec loop n =
    Await (function
        | Next a -> loop (n + 1)
        | Stop   -> return n) in
  loop 0

let rec drop n =
  if n = 0
  then cat
  else Await (fun a -> drop (n - 1))

let last =
  let rec loop r =
    Await (function
        | Some a -> loop (Some a)
        | None   -> return r) in
  loop None

let to_list' producer =
  let rec go acc s =
    match s with
    | Close         -> Value acc
    | Value _       -> Value acc
    | Yield (o, s') -> go (o::acc) s'
    | Await k       -> failwith "should be closed"
  in go [] producer


let sum () =
  let rec loop r =
    Await (function
        | Some a -> loop (r + a)
        | None   -> return r) in
  loop 0

(* let rec mux t = *)
  (* match t with *)
  (* | Yield (o, t') -> (Yield (o, t'), Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | Yield (o1, t1'), Yield (o2, t2') -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



