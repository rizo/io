
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

type ('i, 'o, 'r) flow = ('i, 'o, 'r) node lazy_t
 and ('i, 'o, 'r) node =
  | Yield of ('o  * ('i, 'o, 'r) flow)
  | Await of ('i -> ('i, 'o, 'r) flow)
  | Ready of 'r

(* Type Synonyms *)

(* 'Effect's neither `await` nor `yield`. *)
type 'r effect = (void, void, 'r) flow

(* 'Producer's can only `yield`. *)
type ('o, 'r) producer = (void, 'o, 'r) flow

(* 'Consumer's can only `await`. *)
type ('i, 'r) consumer = ('i, void, 'r) flow

(* Monad *)

let return r = lazy (Ready r)

let rec (>>=) (lazy n) f =
  match n with
  | Yield (b, n') -> lazy (Yield (b, n' >>= f))
  | Await k       -> lazy (Await (fun a -> k a >>= f))
  | Ready r       -> f r

let (>>) n1 n2 =
  n1 >>= fun _ -> n2

(* Category *)

let rec id =
  lazy (Await (fun a -> lazy (Yield (a, id))))

let rec compose n1 n2 =
  match force n1, force n2 with
  | Ready r        , _              -> lazy (Ready r)
  | Yield (b, n1') , _              -> lazy (Yield (b, compose n1' n2))
  | Await k        , Yield (b, n2') -> compose (k b) n2'
  | Await _        , Await k        -> lazy (Await (fun a -> compose n1 (k a)))
  | Await _        , Ready r        -> lazy (Ready r)

let (<<<) n1 n2 = compose n1 n2
let (>>>) n2 n1 = compose n1 n2

(* Creation *)

let empty   = lazy (Ready ())
let yield b = lazy (Yield (b, empty))
let await   = lazy (Await (fun b -> lazy (Ready b)))

(* Helper Operations *)

let rec run n =
  match force n with
  | Ready r       -> r
  | Await k       -> run (k Void)
  | Yield (a, n') -> run n'

let next (lazy node) =
  match node with
  | Ready _       -> None
  | Yield (a, s') -> Some (a, s')
  | Await k       -> fail "Node requires more input."


module Seq
  : sig
    val count : (void, int, 'r) flow
    val map : ('a -> 'b) -> ('a, 'b, 'r) flow
    val filter : ('a -> bool) -> ('a, 'a, 'r) flow
    val take : int -> ('a, 'a, unit) flow
    val take_while : ('a -> bool) -> ('a, 'a, unit) flow
    val drop : int -> ('a, 'a, 'b) flow
    val drop_while : ('a -> bool) -> ('a, 'a, 'b) flow
    val tail : ('a, 'a, 'b) node
    val repeat : 'a -> ('b, 'a, 'c) flow
    val iota : int -> (void, int, unit) flow
    val range : int -> int -> (void, int, unit) flow
    val fold : init:'a -> f:('a -> 'b -> 'a) -> ('c, 'b, 'd) flow -> 'a
    val list : 'a list -> (void, 'a, unit) flow
    val file : string -> (void, string, unit) flow
    val collect : ('a, 'b, 'c) flow -> 'b list
  end

= struct

  let rec count =
    let rec loop n =
      lazy (Yield (n, loop (n + 1))) in
    loop 0

  let rec map f =
    lazy (Await (fun a -> lazy (Yield (f a, map f))))

  let rec filter pred =
    lazy (Await (fun a ->
        if pred a then lazy (Yield (a, filter pred))
        else filter pred))

  let rec take n =
    if n < 0 then raise (Invalid_argument "take: negative index")
    else lazy (if n = 0 then Ready ()
               else Await (fun i -> lazy (Yield (i, take (n - 1)))))

  let rec take_while pred =
    lazy (Await (fun a ->
        lazy (if pred a then Yield (a, take_while pred)
              else Ready ())))

  let rec drop n =
    if n = 0 then id
    else lazy (Await (fun a -> drop (n - 1)))

  let rec drop_while pred =
    lazy (Await (fun a ->
        if pred a then drop_while pred
        else id))

  let tail = Await (fun _ -> id)

  let rec repeat x = lazy (Yield (x, repeat x))

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
    lazy begin match xs with
      | x::xs' -> Yield (x, list xs')
      | []     -> Ready ()
    end

  let rec file file_path =
    let c = open_in file_path in
    let rec loop () =
      lazy (Yield (input_line c, loop ())) in
    try loop ()
    with End_of_file -> empty

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

