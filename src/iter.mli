
(* Iteratee input type. *)
type 'a input =
  (* Input item that may be consumed by iteratee. *)
  | Item of 'a

  (* No input available yet, wait. *)
  | Empty

  (* Input terminated. *)
  | End


(* Iter is the iteratee type.
   An iteratee consumes the input until it either yields a value or encounters
   an error. *)
type ('a, 'b) t =
  (* The iteratee cannot receive any more input, and has computed a result.
     The left-over input is included in the value, which can be passed to the
     next iteratees. *)
  | Yield of 'b * 'a input

  (* The iteratee accepts the input item and performs the calculation. *)
  | Continue of ('a input -> ('a, 'b) t)

	(* The 'Iteratee' encountered an error which prevents it from proceeding. *)
  | Error of exn

type ('a, 'b) iter = ('a, 'b) t

type ('a, 'b) enum = ('a, 'b) iter -> ('a, 'b) iter

val enum_list : 'a list -> ('a, 'b) iter -> ('a, 'b) iter

val run : ('a, 'b) iter -> 'b option
val run_exn : ('a, 'b) iter -> 'b

(*val head : ('a, 'a) iter*)

val drop : int -> ('a, unit) iter

(* Count how many elements remaine in the input.
   Consumes the entire input. *)
val length : ('a, int) iter

(* Get the last element of the input, or 'None' if the input has ended.
   Consumes the entire input. *)
val last : ('a, 'a option) iter

val pure : 'b -> ('a, 'b) iter

val bind  : ('a, 'b) iter -> ('b -> ('a, 'c) iter) -> ('a, 'c) iter
val (>>=) : ('a, 'b) iter -> ('b -> ('a, 'c) iter) -> ('a, 'c) iter

val seq  : ('a, 'b) iter -> ('a, 'c) iter -> ('a, 'c) iter
val (>>) : ('a, 'b) iter -> ('a, 'c) iter -> ('a, 'c) iter

val map  : ('a -> 'b) -> ('a, 'b list) iter

val fmap : ('a -> 'b) -> ('c, 'a) iter -> ('c, 'b) iter


