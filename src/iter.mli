
module Input :
  sig
    (* Iteratee input type. *)
    type 'a t =
      (* Input item that may be consumed by stepatee. *)
      | Item of 'a

      (* No input available yet, wait. *)
      | Empty

      (* Input terminated. *)
      | End
    val show : ('a -> bytes) -> 'a t -> bytes
    val map : ('a -> 'a) -> 'a t -> 'a t
  end

type 'a input = 'a Input.t

(* step is the stepatee type.
   An stepatee consumes the input until it either yields a value or encounters
   an error. *)
type ('a, 'b) t =
  (* The stepatee cannot receive any more input, and has computed a result.
     The left-over input is included in the value, which can be passed to the
     next stepatees. *)
  | Yield of 'b * 'a input

  (* The stepatee accepts the input item and performs the calculation. *)
  | Await of ('a input -> ('a, 'b) t)

	(* The 'stepatee' encountered an error which prevents it from proceeding. *)
  | Error of exn

type ('a, 'b) step = ('a, 'b) t
type ('a, 'b) iter = ('a, 'b) step

type ('a, 'b) enum = ('a, 'b) step -> ('a, 'b) step

val enum_list : 'a list -> ('a, 'b) step -> ('a, 'b) step

val run : ('a, 'b) step -> 'b option
val run_exn : ('a, 'b) step -> 'b

(*val head : ('a, 'a) step*)

val drop : int -> ('a, unit) step

(* Count how many elements remaine in the input.
   Consumes the entire input. *)
val length : ('a, int) step

(* Get the last element of the input, or 'None' if the input has ended.
   Consumes the entire input. *)
val last : ('a, 'a option) step

val pure : 'b -> ('a, 'b) step

val bind  : ('a, 'b) step -> ('b -> ('a, 'c) step) -> ('a, 'c) step
val (>>=) : ('a, 'b) step -> ('b -> ('a, 'c) step) -> ('a, 'c) step

val seq  : ('a, 'b) step -> ('a, 'c) step -> ('a, 'c) step
val (>>) : ('a, 'b) step -> ('a, 'c) step -> ('a, 'c) step

(* Transformer *)
val map : ('a -> 'b) -> ('a, 'r) enum
val map_enum : ('a -> 'a) -> ('a, 'b) iter -> ('a, 'b) iter

val fmap : ('a -> 'b) -> ('c, 'a) step -> ('c, 'b) step


