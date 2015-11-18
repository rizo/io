
open Elements

(*

      +---------+     +---------+     +---------+
      |         |     |         |     |         |
 ... --> node0 --> i --> node1 --> o --> nnde2 --> ...
      |         |     |         |     |         |
      +----|----+     +----|----+     +----|----+
           v               v               v
          ...              r              ...
*)

(* Core Types *)

type ('i, 'o, 'r) node =
  | Yield of ('o  * ('i, 'o, 'r) node lazy_t)
  | Await of ('i -> ('i, 'o, 'r) node)
  | Ready of 'r

(* Type Synonyms *)

(* 'Effect's neither `await` nor `yield`. *)
type 'r effect = (void, void, 'r) node

(* 'Producer's can only `yield`. *)
type ('o, 'r) producer = (void, 'o, 'r) node

(* 'Consumer's can only `await`. *)
type ('i, 'r) consumer = ('i, void, 'r) node

(* Monad *)

let return r = Ready r

let rec (>>=) n f =
  match n with
  | Yield (b, lazy n') -> Yield (b, lazy (n' >>= f))
  | Await k            -> Await (fun a -> k a >>= f)
  | Ready r            -> f r

let (>>) n1 n2 =
  n1 >>= fun _ -> force n2

(* Creation *)

let empty   = Ready ()
let yield b = Yield (b, lazy empty)
let await   = Await (fun b -> Ready b)

(* Category *)

let rec id    = Await (fun a -> Yield (a, lazy id))
let rec id () = await >>= fun a -> yield a >> lazy (id ())

let rec compose n1 n2 =
  match n1, n2 with
  | Ready r             , _                   -> Ready r
  | Yield (b, lazy n1') , _                   -> Yield (b, lazy (compose n1' n2))
  | Await k             , Yield (b, lazy n2') -> compose (k b) n2'
  | Await _             , Await k             -> Await (fun a -> compose n1 (k a))
  | Await _             , Ready r             -> Ready r

let (<<<) n1 n2 = compose n1 n2
let (>>>) n2 n1 = compose n1 n2

(* Helper Operations *)

let rec run n =
  match n with
  | Ready r            -> r
  | Await k            -> run (k Void)
  | Yield (a, lazy n') -> run n'

let next node =
  match node with
  | Ready _            -> None
  | Yield (a, lazy s') -> Some (a, s')
  | Await k            -> fail "Node requires more input."


module Seq
  : sig
    val count : (void, int, 'r) node
    val map : ('a -> 'b) -> ('a, 'b, 'r) node
    val filter : ('a -> bool) -> ('a, 'a, 'r) node
    val take : int -> ('a, 'a, unit) node
    val take_while : ('a -> bool) -> ('a, 'a, unit) node
    val drop : int -> ('a, 'a, 'b) node
    val drop_while : ('a -> bool) -> ('a, 'a, 'b) node
    val tail : unit -> ('a, 'a, 'b) node
    val repeat : 'a -> ('b, 'a, 'c) node
    val iota : int -> (void, int, unit) node
    val range : int -> int -> (void, int, unit) node
    val fold : init:'a -> f:('a -> 'b -> 'a) -> ('c, 'b, 'd) node -> 'a
    val list : 'a list -> (void, 'a, unit) node
    val file : string -> (void, string, unit) node
    val collect : ('a, 'b, 'c) node -> 'b list
  end

= struct

  let rec count =
    let rec loop n =
      yield n >> lazy (loop (n + 1)) in
    loop 0

  let rec map f =
    await >>= fun a -> yield (f a) >> lazy (map f)

  let rec filter pred =
    await >>= fun a ->
        if pred a then yield a >> lazy (filter pred)
        else filter pred

  let rec take n =
    if n < 0 then raise (Invalid_argument "take: negative index")
    else if n = 0 then return ()
    else await >>= fun i -> yield i >> lazy (take (n - 1))

  let rec take_while pred =
    await >>= fun a ->
    if pred a then yield a >> lazy (take_while pred)
    else return ()

  let rec drop n =
    if n = 0 then id ()
    else await >>= fun a -> drop (n - 1)

  let rec drop_while pred =
    await >>= fun a ->
    if pred a then drop_while pred
    else id ()

  let tail () = await >>= fun _ -> id ()

  let rec repeat x = yield x >> lazy (repeat x)

  let rec iota stop =
    count >>> take stop

  let rec range start stop =
    count >>> take stop >>> drop start

  let fold ~init ~f source =
    let rec loop source acc =
      match next source with
      | Some (a, rest) -> loop rest (f acc a)
      | None           -> acc in
    loop source init

  let rec list xs =
    match xs with
    | x::xs' -> yield x >> lazy (list xs')
    | []     -> return ()

  let rec file file_path =
    let c = open_in file_path in
    let rec loop () =
      yield (input_line c) >> lazy (loop ()) in
    try loop ()
    with End_of_file -> return ()

  let collect src =
    let rec loop src acc =
      match next src with
      | Some (a, rest) -> loop rest (a::acc)
      | None -> List.rev acc
    in loop src []

end


module Test = struct
  open Seq

  let general () =
    let result = fold ~f:(+) ~init:0
        (iota 100000 >>> map ((+) 100) >>> take_while ((>) 108)) in
    assert (result = 828)
end

