
open Elements

type ('a, 'b, 'r) stream = unit -> ('a, 'b, 'r) stream_state
 and ('a, 'b, 'r) stream_state =
  | Ready of 'r
  | Error of exn
  | Yield of ('b  * ('a, 'b, 'r) stream)
  | Await of ('a -> ('a, 'b, 'r) stream)

type          'b source = (void, 'b, unit) stream
type ('a, 'b) processor = ('a,   'b, unit) stream
type ('b, 'a)      sink = 'b source -> 'a

let return x =
  fun () -> Ready x

let rec (>>=) s f =
  match s () with
  | Error e       -> fun () -> Error e
  | Ready r       -> f r
  | Yield (o, s') -> fun () -> Yield (o, s' >>= f)
  | Await k       -> fun () -> Await (fun i -> k i >>= f)

let (>>) s1 s2 =
  s1 >>= fun _ -> s2

let yield b =
  fun () -> Yield (b, return ())

(* FIXME: yield 4 >> (fun () -> Await fun a -> return (a + 1)) => collect *)
let await =
  fun () -> Await (fun a -> return a)

let rec compose s1 s2 =
  match s1 (), s2 () with
  | _              , Error e        -> fun () -> Error e
  | Error e        , _              -> fun () -> Error e
  | Ready r        , _              -> return r
  | Yield (b, s1') , _              -> fun () -> Yield (b, compose s1' s2)
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> fun () -> Await (fun a -> compose s1 (k a))
  | Await _        , Ready r        -> return r

let (=<=) s1 s2 = compose s1 s2
let (=>=) s2 s1 = compose s1 s2
let (=>) x f = f x

let rec cat =
  fun () -> Await (fun i -> fun () -> Yield (i, cat))

let rec run s =
  match s () with
  | Ready r       -> r
  | Error e       -> raise e
  | Await k       -> run (k Void)
  | Yield (a, s') -> run s'

let next source : ('a * 'a source) option =
  match source () with
  | Error e       -> raise e
  | Ready ()      -> None
  | Yield (a, s') -> Some (a, s')
  | Await k       -> fail "stream is still awaiting input"

let rec of_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> fun () -> Yield (line, of_chan ch)
  | None      -> return ()

let rec of_list xs =
  match xs with
  | x::xs' -> fun () -> Yield (x, of_list xs')
  | []     -> return ()

let rec map f =
  fun () -> Await (fun a -> fun () -> Yield (f a, map f))

let rec filter pred =
  fun () -> Await (fun a ->
      if pred a
      then fun () -> Yield (a, filter pred)
      else filter pred)

let rec take n =
  if n < 0 then
    fun () -> Error (Invalid_argument "take: negative value")
  else if n = 0
  then return ()
  else fun () -> Await (fun i -> fun () -> Yield (i, take (n - 1)))

let rec drop n =
  if n = 0
  then cat
  else fun () -> Await (fun a -> drop (n - 1))

let tail =
  fun () -> Await (fun a -> cat)

let rec print =
  map print_endline


let rec repeat x : 'a source =
  fun () -> Yield (x, repeat x)

let yes : string source = repeat "y"

let rec count () =
  let rec go n =
    Yield (n, fun () -> go (n + 1)) in
  go 0

let rec range start stop : int source =
  count =>= take stop =>= drop start

let rec range n m : int source =
  count =>= take m =>= drop n

let rec iota stop : int source =
  count =>= take stop

let fold f z source =
  let rec go acc source =
    match next source with
    | Some (a, rest) -> go (f acc a) rest
    | None           -> acc in
  go z source

let nth n source =
  if n < 0 then
    error (Invalid_argument "nth: negative index")
  else
    let rec loop n source =
      match next source with
      | None           -> error (Failure "nth: empty stream")
      | Some (a, rest) -> if n = 0 then ok a else loop (n - 1) rest
    in loop n source

let collect src =
  let rec go acc src =
    match next src with
    | Some (a, rest) -> go (a::acc) rest
    | None -> List.rev acc
  in go [] src

let head p =
  match next p with
  | Some (a, _) -> Ok a
  | None -> Error (Failure "No head")

let len p =
  let rec go total p =
    match next p with
    | Some (_, p') -> go (total + 1) p'
    | None -> total in
  go 0 p

let last p =
  let rec loop last_opt p =
    match next p with
    | Some (a, p') -> loop (Some a) p'
    | None         -> last_opt in
  loop None p

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



