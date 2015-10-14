
open Elements

(* Stream represents a state of continuous computation. *)
type ('a, 'b, 'r) stream = unit -> ('a, 'b, 'r) stream_state
 and ('a, 'b, 'r) stream_state =
   | Ready of 'r
   | Yield of ('b  * ('a, 'b, 'r) stream)
   | Await of ('a -> ('a, 'b, 'r) stream)

(* Source is a stream that can only `yield`. *)
type 'b source = (void, 'b, unit) stream

(* Processor applies a transformation to each element of the stream. *)
type ('a, 'b) processor = ('a, 'b, unit) stream

(* Sink is a stream that can only `await` and `return` a result when done. *)
type ('b, 'a) sink = 'b source -> 'a

val return : 'r -> ('a, 'b, 'r) stream

val ( >>= ) : ('a, 'b, 'r) stream -> ('r -> ('a, 'b, 's) stream) -> ('a, 'b, 's) stream

val ( >> )  : ('a, 'b, 'r) stream -> ('a, 'b, 's) stream -> ('a, 'b, 's) stream

val yield : 'a -> ('b, 'a, unit) stream

val await : ('a, 'b, 'a) stream

val compose : ('a, 'b, 'r) stream -> ('d, 'a, 'r) stream -> ('d, 'b, 'r) stream
val ( =<= ) : ('a, 'b, 'r) stream -> ('d, 'a, 'r) stream -> ('d, 'b, 'r) stream
val ( =>= ) : ('a, 'b, 'r) stream -> ('b, 'd, 'r) stream -> ('a, 'd, 'r) stream

val ( => ) : 'a -> ('a -> 'b) -> 'b

val cat : ('a, 'a, void) stream

val run : (void, void, 'b) stream -> 'b

val next : 'a source -> ('a * 'a source) option

val get_line_from_chan : in_channel -> ('a, string, unit) stream

val get_char_from_chan : in_channel -> ('a, char, unit) stream

val of_list : 'a list -> ('b, 'a, unit) stream

val collect : 'a source -> 'a list

val count : 'a source -> int

val drop : int -> ('a, 'a) processor

val filter : ('a -> bool) -> ('a, 'a, 'b) stream

val fold : ('a -> 'b -> 'a) -> 'a -> 'b source -> 'a

val head : 'a source -> ('a, exn) result

val infinity : int source

val iota : int -> int source

val last : 'a source -> 'a option

val map : ('a -> 'b) -> ('a, 'b, 'c) stream

val nth : int -> 'a source -> ('a, exn) result

val print : string source -> unit

val range : int -> int -> int source

val repeat : 'a -> 'a source

val sum : int source -> int

val tail : ('a, 'a) processor

val take : int -> ('a, 'a) processor

val yes : string source

