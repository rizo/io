
open Elements

type stop = unit -> unit

type ('a, 'b, 'r) node =
  | Yield of ('b  * ('a, 'b, 'r) node lazy_t)
  | Await of ('a -> ('a, 'b, 'r) node)
  | Ready of  'r

type 'r effect = (void, void, 'r) node

type ('b, 'r) producer = (void, 'b, 'r) node

type ('a, 'r) consumer = ('a, void, 'r) node

val return : 'r -> ('a, 'b, 'r) node

val ( >>= ) :
  ('a, 'b, 'r1) node -> ('r1 -> ('a, 'b, 'r2) node) -> ('a, 'b, 'r2) node

val ( >> ) :
  ('a, 'b, 'r1) node -> ('a, 'b, 'r2) node lazy_t -> ('a, 'b, 'r2) node

val forever : ('a, 'b, 'r1) node -> ('a, 'b, 'r2) node

val replicate : int -> ('a, 'b, 'r) node -> ('a, 'b, unit) node

val empty : ('a, 'b, unit) node

val yield : 'b -> ('a, 'b, unit) node

val await : ('a, 'b, 'a) node

val id : ('a, 'a, 'r) node

val compose : ('b, 'c, 'r) node -> ('a, 'b, 'r) node -> ('a, 'c, 'r) node

val ( <<< ) : ('b, 'c, 'r) node -> ('a, 'b, 'r) node -> ('a, 'c, 'r) node

val ( >>> ) : ('a, 'b, 'r) node -> ('b, 'c, 'r) node -> ('a, 'c, 'r) node

val run : ('a, 'b, 'r) node -> 'r option

val next : ('a, 'b, 'r) node -> ('b * ('a, 'b, 'r) node) option

