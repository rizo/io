
open Elements

type ('a, 'b, 'r) stream = unit -> ('a, 'b, 'r) stream_state
 and ('a, 'b, 'r) stream_state =
  | Ready of 'r
  | Error of exn
  | Yield of ('b  * ('a, 'b, 'r) stream)
  | Await of ('a -> ('a, 'b, 'r) stream)

type (        'r) workflow    = (void, void, 'r) stream
type (    'o, 'r) producer    = (void,   'o, 'r) stream
type ('i, 'o, 'r) transformer = ('i,     'o, 'r) stream
type ('i,     'r) consumer    = ('i, 'r) producer -> 'r workflow

let return x =
  fun () -> Ready x

let rec cat =
  fun () -> Await (fun i -> fun () -> Yield (i, cat))

let rec compose s1 s2 =
  match s1 (), s2 () with
  | _              , Error e        -> fun () -> Error e
  | Error e        , _              -> fun () -> Error e
  | Ready r        , _              -> fun () -> Ready r
  | Yield (b, s1') , _              -> fun () -> Yield (b, compose s1' s2)
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> fun () -> Await (fun a -> compose s1 (k a))
  | Await _        , Ready r        -> fun () -> Ready r

let (=<=) s1 s2 = compose s1 s2
let (=>=) s2 s1 = compose s1 s2
let (=>) x f = f x

let rec (>>=) s f =
  match s () with
  | Error e       -> fun () -> Error e
  | Ready r       -> f r
  | Yield (o, s') -> fun () -> Yield (o, s' >>= f)
  | Await k       -> fun () -> Await (fun i -> k i >>= f)

let (>>) s1 s2 =
  s1 >>= fun _ -> s2

let yield b =
  fun () -> Yield (b, fun () -> Ready ())

(* FIXME: yield 4 >> (await >>= fun a -> return (a + 1)) => collect *)
let await =
  fun () -> Await (fun a -> fun () -> Ready a)

(* to review *)
let rec run s =
  match s () with
  | Ready r       -> r
  | Error e       -> raise e
  | Await k       -> run (k Void)
  | Yield (a, s') -> run s'

let next producer =
  match producer () with
  | Error e       -> raise e
  | Ready _       -> None
  | Yield (a, s') -> Some (a, s')
  | Await k       -> fail "invalid producer state"

let rec take n =
  if n < 0 then
    fun () -> Error (Invalid_argument "take: negative value")
  else if n = 0
  then return ()
  else fun () -> Await (fun i -> fun () -> Yield (i, take (n - 1)))

let rec map f =
  fun () -> Await (fun a -> fun () -> Yield (f a, map f))

let rec filter pred =
  fun () -> Await (fun a ->
      if pred a
      then fun () -> Yield (a, filter pred)
      else filter pred)

let rec print =
  fun () -> Await (fun a -> print_endline a; print)

let rec print =
  map print_endline

let rec of_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> fun () -> Yield (line, of_chan ch)
  | None      -> return ()

let rec of_list xs =
  match xs with
  | x::xs' -> fun () -> Yield (x, of_list xs')
  | []     -> return ()

(* Producers
 * ========= *)

let rec repeat x : ('a, 'r) producer =
  fun () -> Yield (x, repeat x)

let yes : (string, 'r) producer = repeat "y"

let rec count () =
  let rec go n =
    Yield (n, fun () -> go (n + 1)) in
  go 0

let rec iota n =
  count =>= take n

(* Transducers
 * =========== *)

let rec drop n =
  if n = 0
  then cat
  else fun () -> Await (fun a -> drop (n - 1))

let tail =
  fun () -> Await (fun a -> cat)

(* Consumers
 * ========= *)

let fold f z p () =
  let rec go acc p =
    match next p with
    | Some (a, p') -> go (f acc a) p'
    | None         -> Ready acc in
  go z p

(* val nth : int -> ('a, 'r) producer -> 'a workflow *)
let nth n p () =
  if n < 0 then
    Error (Invalid_argument "nth: negative index")
  else
    let rec loop n p =
      match next p with
      | None         -> Error (Failure "nth: empty stream")
      | Some (a, p') -> if n = 0 then Ready a else loop (n - 1) p'
    in loop n p

let collect p () =
  let rec go acc p =
    match next p with
    | Some (a, p') -> go (a::acc) p'
    | None -> Ready (List.rev acc)
  in go [] p

let head p () =
  match next p with
  | Some (a, _) -> Ready a
  | None -> Error (Failure "No head")

let len p () =
  let rec go total p =
    match next p with
    | Some (_, p') -> go (total + 1) p'
    | None -> Ready total in
  go 0 p

let last p =
  let rec loop last_opt p =
    match next p with
    | Some (a, p') -> loop (Some a) p'
    | None         -> last_opt in
  loop None p

(* let sum p () = *)
  (* let rec go total p = *)
    (* match next p with *)
    (* | Some (a, p') -> go (total + a) p' *)
    (* | None -> Ready total in *)
  (* go 0 p *)

let sum p = fold (+) 0 p

(*
 * Divices
 *)

(* let rec mux t = *)
  (* match t with *)
  (* | fun () -> Yield (o, t') -> (fun () -> Yield (o, t'), fun () -> Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | fun () -> Yield (o1, t1'), fun () -> Yield (o2, t2') -> fun () -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



