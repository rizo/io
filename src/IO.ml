
open Elements

(*

      +---------+       +---------+       +---------+
      |         |       |         |       |         |
 ... --> node0 -->  i  --> node1 -->  o  --> nnde2 --> ...
      |         |       |         |       |         |
      +----|----+       +----|----+       +----|----+
           v                 v                 v
          ...                r                ...
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
  n1 >>= fun _ -> !n2

let rec forever m =
  m >> lazy (forever m)

let rec replicate n m =
  if n < 0 then raise (Invalid_argument "replicate: negative index")
  else if n = 0 then return ()
  else m >> lazy (replicate (n - 1) m)

(* Creation *)

let empty   = Ready ()
let yield b = Yield (b, lazy empty)
let await   = Await (fun b -> Ready b)

(* Category *)

let rec id    = Await (fun a -> Yield (a, lazy id))
(* let rec id () = await >>= fun a -> yield a >> lazy (id ()) *)

let rec compose d u =
  match d, u with
  (* Downstream is ready & upstream still yielding: notify the upstream about termination. *)
  | Ready r       , Yield _       -> Ready r

  | Ready r       , Await _       -> Ready r
  | Ready r       , Ready _       -> Ready r
  | Yield (b, d') , _             -> yield b >> lazy (compose !d' u)
  | Await k       , Yield (b, u') -> compose (k b) !u'
  | Await _       , Await k       -> await >>= fun a -> compose d (k a)

  (* Upstream is ready & downstream is still awaiting: notify downstream about termination. *)
  | Await _       , Ready r       -> Ready r

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
    val ( => ) : ('a, 'b, 'r) node -> ('b, 'c, 'r) node -> ('a, 'c, 'r) node
    val ( <= ) : ('b, 'c, 'r) node -> ('a, 'b, 'r) node -> ('a, 'c, 'r) node
    val count : (void, int, 'r) node
    val nth : int -> ('a, 'b, 'r) node -> 'b option
    val map : ('a -> 'b) -> ('a, 'b, 'r) node
    val filter : ('a -> bool) -> ('a, 'a, 'r) node
    val take : int -> ('a, 'a, unit) node
    val take_while : ('a -> bool) -> ('a, 'a, unit) node
    val drop : int -> ('a, 'a, 'b) node
    val drop_while : ('a -> bool) -> ('a, 'a, 'b) node
    val tail : ('a, 'a, 'b) node
    val repeat : ?n:int -> 'a -> ('b, 'a, unit) node
    val iota : int -> (void, int, unit) node
    val slice : int -> int -> ('a, 'a, unit) node
    val range : int -> int -> (void, int, unit) node
    val fold : init:'a -> f:('a -> 'b -> 'a) -> ('c, 'b, 'd) node -> 'a
    val sum : (void, int, 'r) node -> int
    val len : (void, int, 'r) node -> int
    val any : (void, bool, 'r) node -> bool
    val last : (void, 'b, 'r) node -> 'b option
    val head : (void, 'b, 'r) node -> 'b option
    val list : 'a list -> (void, 'a, unit) node
    val file : string -> (void, string, unit) node
    val collect : ('a, 'b, 'c) node -> 'b list
  end
= struct

  let (=>) = (>>>)
  let (<=) = (<<<)

  let rec count =
    let rec loop n =
      yield n >> lazy (loop (n + 1)) in
    loop 0

  let rec map_rec f =
    await >>= fun a -> yield (f a) >> lazy (map_rec f)

  let map_forever f =
    forever (await >>= fun a -> yield (f a))

  let map = map_forever

  let rec filter pred =
    await >>= fun a ->
        if pred a then yield a >> lazy (filter pred)
        else filter pred

  let rec take_rec n =
    if n < 0 then raise (Invalid_argument "take: negative index")
    else if n = 0 then return ()
    else await >>= fun i -> yield i >> lazy (take_rec (n - 1))

  let take_replicate n =
    replicate n (await >>= yield)

  let take = take_replicate

  let rec take_while pred =
    await >>= fun a ->
    if pred a then yield a >> lazy (take_while pred)
    else return ()

  let rec drop n =
    if n = 0 then id
    else await >>= fun a -> drop (n - 1)

  let rec drop_while pred =
    await >>= fun a ->
    if pred a then drop_while pred
    else id

  let tail = Await (fun _ -> id)

  let rec repeat ?n x =
    match n with
    | Some n -> replicate n (yield x)
    | None -> forever (yield x)

  let rec iota stop =
    count >>> take stop

  let range start stop =
    count >>> take stop >>> drop start

  let slice i j =
    drop i >>> take (j - i)

  let fold ~init ~f source =
    let rec loop source acc =
      match next source with
      | Some (a, rest) -> loop rest (f acc a)
      | None           -> acc in
    loop source init

  let nth_direct n source =
    if n < 0 then fail "nth: negative index"
    else
      let rec loop n source =
        match next source with
        | Some (a, rest) ->
          if n = 0 then Some a
          else loop (n - 1) rest
        | None -> None
      in loop n source

  let nth = nth_direct

  let head p =
    match next p with
    | Some (a, _) -> Some a
    | None        -> None

  let sum source = fold ~init:0 ~f:(+) source
  let len source = fold ~init:0 ~f:(fun c i -> c + 1) source

  let rec any source =
    match next source with
    | Some (a, _) when a -> a
    | Some (a, rest)     -> any rest
    | None               -> false

  let last source =
    let rec loop last_opt source =
      match next source with
      | Some (a, rest) -> loop (Some a) rest
      | None           -> last_opt in
    loop None source

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

  let test_slice () = begin
    assert (collect (count => Seq.slice 3 8) = [3; 4; 5; 6; 7]);
  end


  let api () = begin
    (* Compute the sum of all odd integers up to 1000000. *)
    assert (fold ~init:0 ~f:(+) (iota 1000000 => filter odd) = 250000000000);

    (* Take 5 integers from an infinit sequence and collect them into a list. *)
    assert (collect (count => take 5) = [0; 1; 2; 3; 4]);
  end
end

