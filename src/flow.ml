

open Elements

let rec closed : void -> 'a
  = fun x -> closed x

type ('i, 'o) stream = unit -> ('i, 'o) stream_state
 and ('i, 'o) stream_state =
  | Empty
  | Error of exn
  | Yield of ('o  * ('i, 'o) stream)
  | Await of ('i -> ('i, 'o) stream)

type      'o  source = (void, 'o) stream
type ('i, 'a)   sink = 'i source -> 'a

let return x =
  fun () -> Yield (x, fun () -> Empty)

let rec cat =
  fun () -> Await (fun i -> fun () -> Yield (i, cat))

let rec compose s1 s2 =
  match s1 (), s2 () with
  | _              , Error e        -> fun () -> Error e
  | Error e        , _              -> fun () -> Error e
  | Empty          , _              -> fun () -> Empty
  | Yield (b, s1') , _              -> fun () -> Yield (b, compose s1' s2)
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> fun () -> Await (fun a -> compose s1 (k a))
  | Await _        , Empty          -> fun () -> Empty

let (=<=) s1 s2 = compose s1 s2
let (=>=) s2 s1 = compose s1 s2
let (=>) x f = f x

let rec (>>=) s f =
  match s () with
  | Error e       -> fun () -> Error e
  | Empty         -> f ()
  | Yield (o, s') -> fun () -> Yield (o, s' >>= f)
  | Await k       -> fun () -> Await (fun i -> k i >>= f)

let (>>) s1 s2 =
  s1 >>= fun () -> s2

let yield b =
  fun () -> Yield (b, fun () -> Empty)

let await =
  fun () -> Await (fun a -> fun () -> Yield (a, fun () -> Empty))

(* to review *)
let rec run s =
  match s () with
  | Empty            -> fail "empty stream"
  | Error e          -> raise e
  | Await k          -> run (k Void)
  | Yield (a, s') when s' = fun () -> Empty -> a
  | Yield (a, s') -> run s'

let next source =
  match source () with
  | Error e       -> raise e
  | Empty         -> None
  | Yield (a, s') -> Some (a, s')
  | Await k       -> fail "invalid producer state"

let rec take n =
  if n < 0 then
    fun () -> Error (Invalid_argument "take: negative value")
  else if n = 0
  then fun () -> Empty
  else fun () -> Await (fun i -> fun () -> Yield (i, take (n - 1)))

let rec map f =
  fun () -> Await (fun a -> fun () -> Yield (f a, map f))

let rec filter pred =
  fun () -> Await (fun a ->
      if pred a
      then fun () -> Yield (a, filter pred)
      else filter pred)

let fold f z p =
  let rec go acc p =
    match next p with
    | Some (a, p') -> go (f acc a) p'
    | None         -> acc in
  go z p

let rec print =
  fun () -> Await (fun a -> print_endline a; print)

let rec print =
  map print_endline

let rec source_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> fun () -> Yield (line, source_chan ch)
  | None      -> fun () -> Empty

let rec source_list xs =
  match xs with
  | x::xs' -> fun () -> Yield (x, source_list xs')
  | []     -> fun () -> Empty

let rec repeat x =
  fun () -> Yield (x, repeat x)

let yes : string source = repeat "y"

let rec count () =
  let rec go n =
    Yield (n, fun () -> go (n + 1)) in
  go 0

let rec iota n =
  count =>= take n

(* just for comparison *)
let nth_list n l =
  if n < 0 then
    raise (Invalid_argument "nth: negative index")
  else
    let rec loop n l =
      match l with
      | [] -> raise (Failure "nth: index out of rande")
      | a::l -> if n = 0 then a else loop (n - 1) l
    in loop n l

let nth n p =
  if n < 0 then
    raise (Invalid_argument "nth: negative index")
  else
    let rec loop n p =
      match next p with
      | None         -> raise (Failure "nth: empty stream")
      | Some (a, p') -> if n = 0 then a else loop (n - 1) p'
    in loop n p

let collect p =
  let rec go acc p =
    match next p with
    | Some (a, p') -> go (a::acc) p'
    | None -> List.rev acc
  in go [] p

let head p =
  match next p with
  | Some (a, _) -> Some a
  | None -> None

let len p =
  let rec go total p =
    match next p with
    | Some (_, p') -> go (total + 1) p'
    | None -> total in
  go 0 p

let rec drop n =
  if n = 0
  then cat
  else fun () -> Await (fun a -> drop (n - 1))

let tail =
  fun () -> Await (fun a -> cat)

let last p =
  let rec loop last_opt p =
    match next p with
    | Some (a, p') -> loop (Some a) p'
    | None         -> last_opt in
  loop None p

let sum p = fold (+) 0 p
let sum p =
  let rec go total p =
    match next p with
    | Some (a, p') -> go (total + a) p'
    | None -> total in
  go 0 p

(* let rec mux t = *)
  (* match t with *)
  (* | fun () -> Yield (o, t') -> (fun () -> Yield (o, t'), fun () -> Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | fun () -> Yield (o1, t1'), fun () -> Yield (o2, t2') -> fun () -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



