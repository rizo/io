
open Elements

(*

      +--------------+
      |              |
 a2  <--            <--  b2
      |     node0    |
 a   -->            -->  b
      |              |
      +-------|------+
              v

              r
*)

type ('a2, 'a, 'b2, 'b, 'r) node =
  | Request of ('a2 * ('a  -> ('a2, 'a, 'b2, 'b, 'r) node))
  | Respond of ('b  * ('b2 -> ('a2, 'a, 'b2, 'b, 'r) node))
  | Pure    of 'r

let pure x = Pure x
let return = pure

let rec (>>=) n0 f =
  let rec go n =
    match n with
    | Request (a', fa) -> Request (a', fun a -> go (fa a))
    | Respond (b, fb') -> Respond (b, fun b' -> go (fb' b'))
    | Pure r           -> f r in
  go n0

let (>>) s1 s2 =
  s1 >>= fun _ -> s2

let request a = Request (a, pure)
let respond a = Respond (a, pure)

let each n0 fb =
  let rec go n =
    match n with
    | Request (x', fx) -> Request (x', go @. fx)
    | Respond (b, fb') -> fb b >>= go @. fb'
    | Pure r           -> Pure r in
  go n0

let (//>) n f = each n f
let (/>/) f g a = f a //> g
let (~>) = (/>/)

let rev_each fb' n0 =
  let rec go n =
    match n with
    | Request (b', fb) -> fb' b' >>= go @. fb
    | Respond (x, fx') -> Respond (x, go @. fx')
    | Pure r           -> Pure r in
  go n0

(* let (>\\) f n = back f n           *)
(* let (\>\) fb' fc' c' = fb' >\\ fc' c' *)
(* let (>~) n1 n2 = (fun () -> n1) >\\ n2 *)

(* The identity of the push category. *)
let rec push a =
  Respond (a, fun a' -> Request (a', push))

(* Point-ful version of ('>+>') *)
let rec (+>>) fb' n =
  match n with
  | Request (b', fb) -> fb' b' >>~ fb
  | Respond (c, fc') -> Respond (c, fun c' -> fb' +>> fc' c')
  | Pure r           -> Pure r

(* Point-ful version of ('>~>') *)
and (>>~) n fb =
  match n with
  | Request (a', fa) -> Request (a', fun a -> fa a >>~ fb)
  | Respond (b, fb') -> fb' +>> fb b
  | Pure r           -> Pure r

(* Composition operator of the push category. *)
let (>~>) fa fb   = fun a  -> fa a >>~ fb

(* The identity of the pull category. *)
let rec pull a' =
  Request (a', fun a -> Respond (a, pull))

(* Composition operator of the pull category. *)
let (>+>) fb' fc' = fun c' -> fb'  +>> fc' c'

(* Switch the upstream and downstream ends. *)
let rec reflect n =
  match n with
  | Request (a', fa) -> Respond (a', fun a -> reflect (fa a))
  | Respond (b, fb') -> Request (b, fun b' -> reflect (fb' b'))
  | Pure r           -> Pure r

(* 'Effect's neither 'await' nor 'yield' *)
type 'r effect = (void, unit, unit, void, 'r) node

(* 'Producer's can only 'yield' *)
type ('b, 'r) producer = (void, unit, unit, 'b, 'r) node

(* 'Pipe's can both 'await' and 'yield' *)
type ('a, 'b, 'r) pipe = (unit, 'a, unit, 'b, 'r) node

(* 'Consumer's can only 'await' *)
type ('a, 'r) consumer = (unit, 'a, unit, void, 'r) node

(* 'Client's only 'request' and never 'respond'. *)
type ('a2, 'a, 'r) client = ('a2, 'a, unit, void, 'r) node

(* 'Server's only 'respond' and never 'request'. *)
type ('b2, 'b, 'r) server = (void, unit, 'b2, 'b, 'r) node

let yield x = respond x

let await   = Request ((), pure)

(* let cat = pull () *)
let rec cat =
  Request ((), fun a -> Respond (a, pull))

let (>->) n1 n2 = (fun () -> n1) +>> n2

module Seq = struct
  let seq f = each cat f

  let list xs = List.foldr xs ~f:(fun a n -> yield a >> n) ~init:(return ())

  let iter f = seq (fun a -> f a; return ())
  let map  f = seq (fun a -> yield (f a))

  let filter pred =
    seq (fun a -> if pred a then yield a else return ())

  let rec take_while pred =
    await >>= fun a ->
    if pred a then yield a >> take_while pred
    else return ()

  let concat () = seq list

  let fold ~f ~init ?(stop = Fn.id) source =
    let rec loop n x =
      match n with
      | Request (v, _)  -> closed v
      | Respond (a, fu) -> loop (fu ()) (f x a)
      | Pure    _       -> return (stop x) in
    loop source init
end

