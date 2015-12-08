
open Elements

type ('a, 'b, 'r) stream =
  | Yield of ('b  * ('a, 'b, 'r) stream lazy_t)
  | Await of ('a -> ('a, 'b, 'r) stream)
  | Ready of  'r

type 'r effect = (void, void, 'r) stream

type ('b, 'r) producer = (void, 'b, 'r) stream

type ('a, 'r) consumer = ('a, void, 'r) stream

val return : 'r -> ('a, 'b, 'r) stream

val ( >>= ) :
  ('a, 'b, 'r1) stream -> ('r1 -> ('a, 'b, 'r2) stream) -> ('a, 'b, 'r2) stream

val ( >> ) :
  ('a, 'b, 'r1) stream -> ('a, 'b, 'r2) stream lazy_t -> ('a, 'b, 'r2) stream

val forever : ('a, 'b, 'r1) stream -> ('a, 'b, 'r2) stream

val replicate : int -> ('a, 'b, 'r) stream -> ('a, 'b, unit) stream

val empty : ('a, 'b, unit) stream

val yield : 'b -> ('a, 'b, unit) stream

val await : ('a, 'b, 'a) stream

val await_from : ('a, 'r, 'r) stream -> ('a, 'b, 'r) stream

val id : ('a, 'a, 'r) stream

val compose : ('b, 'c, 'r) stream -> ('a, 'b, 'r) stream -> ('a, 'c, 'r) stream

val ( <<< ) : ('b, 'c, 'r) stream -> ('a, 'b, 'r) stream -> ('a, 'c, 'r) stream

val ( >>> ) : ('a, 'b, 'r) stream -> ('b, 'c, 'r) stream -> ('a, 'c, 'r) stream

val run : ('a, 'b, 'r) stream -> 'r option

val next : ('a, 'b, 'r) stream -> ('b * ('a, 'b, 'r) stream) option

