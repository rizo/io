
open Elements

let (!) = Lazy.(!)

(*

      +---------+       +---------+       +---------+
      |         |       |         |       |         |
 ... --> node0 -->  i  --> node1 -->  o  --> node2 --> ...
      |         |       |         |       |         |
      +----|----+       +----|----+       +----|----+
           v                 v                 v
          ...                r                ...
*)

(* Core Types *)

type ('i, 'o, 'r) stream =
  | Yield of ('o  * ('i, 'o, 'r) stream lazy_t)
  | Await of ('i -> ('i, 'o, 'r) stream)
  | Ready of 'r

(* Type Synonyms *)

(* 'Effect's neither `await` nor `yield`. *)
type 'r effect = (void, void, 'r) stream

(* 'Producer's can only `yield`. *)
type ('o, 'r) producer = (void, 'o, 'r) stream

(* 'Consumer's can only `await`. *)
type ('i, 'r) consumer = ('i, void, 'r) stream

(* Monad & Monadic Combinators *)

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

let rec compose d u =
  match d, u with
  (* Downstream is ready & upstream yielding: notify the upstream about termination. *)
  | Ready r       , Yield _       -> Ready r
  (* Downstream is ready & upstream awaiting: notify the upstream about termination. *)
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

let await_from stream =
  stream >>> Await (fun b -> Ready b)

let rec run n =
  match n with
  | Ready r            -> Some r
  | Await k            -> None
  | Yield (a, lazy n') -> run n'

let next stream =
  match stream with
  | Ready _            -> None
  | Yield (a, lazy s') -> Some (a, s')
  | Await k            -> fail "stream requires more input."

