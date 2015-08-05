
(* Input type. *)
type 'a input =
  | Item of 'a
  | Empty
  | End

(* Iteratee computation state. *)
type ('a, 'b) iter =
  | Done of 'b * 'a input
  | Next of ('a input -> ('a, 'b) iter)

type ('a, 'b) t = ('a, 'b) iter

val enum : ('a, 'b) iter -> 'a list -> ('a, 'b) iter

val run : ('a, 'b) iter -> 'b option

val head : ('a, 'a option) iter

val drop : int -> ('a, unit) iter

val length : ('a, int) iter

val pure : ('a, 'b) iter

val bind  : ('a, 'b) iter -> ('b -> ('a, 'c) iter) -> ('a, 'c) iter
val (>>=) : ('a, 'b) iter -> ('b -> ('a, 'c) iter) -> ('a, 'c) iter

val seq  : ('a, 'b) iter -> ('a, 'c) iter -> ('a, 'c) iter
val (>>) : ('a, 'b) iter -> ('a, 'c) iter -> ('a, 'c) iter

val map : ('a -> 'b) -> ('c, 'a) iter -> ('c, 'b) iter


