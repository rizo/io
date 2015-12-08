
open Elements
open IO_core

val ( <= ) : ('b, 'c, 'r) stream -> ('a, 'b, 'r) stream -> ('a, 'c, 'r) stream
val ( => ) : ('a, 'b, 'r) stream -> ('b, 'c, 'r) stream -> ('a, 'c, 'r) stream
val any : (void, bool, 'r) stream -> bool
val chan : in_channel -> ('a, bytes, unit) stream
val collect : ('a, 'b, 'c) stream -> 'b list
val count : (void, int, 'r) stream
val drop : int -> ('a, 'a, 'b) stream
val drop_while : ('a -> bool) -> ('a, 'a, 'b) stream
val each : ('a -> unit) -> ('a, void, 'r) stream
val file : string -> (void, string, unit) stream
val filter : ('a -> bool) -> ('a, 'a, 'r) stream
val filter_map : ('a -> 'b option) -> ('a, 'b, 'r) stream
val fold : init:'a -> f:('a -> 'b -> 'a) -> ('c, 'b, 'd) stream -> 'a
val head : (void, 'b, 'r) stream -> 'b option
val iota : int -> (void, int, unit) stream
val last : (void, 'b, 'r) stream -> 'b option
val length : (void, 'b, 'r) stream -> int
val list : 'a list -> (void, 'a, unit) stream
val map : ('a -> 'b) -> ('a, 'b, 'r) stream
val nth : int -> ('a, 'b, 'r) stream -> 'b option
val range : int -> int -> (void, int, unit) stream
val repeat : ?n:int -> 'a -> ('b, 'a, unit) stream
val slice : int -> int -> ('a, 'a, unit) stream
val sum : (void, int, 'r) stream -> int
val tail : ('a, 'a, 'b) stream
val take : int -> ('a, 'a, unit) stream
val take_while : ('a -> bool) -> ('a, 'a, unit) stream

