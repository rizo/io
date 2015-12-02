
open Elements
open IO_core

val ( <= ) : ('b, 'c, 'r) node -> ('a, 'b, 'r) node -> ('a, 'c, 'r) node
val ( => ) : ('a, 'b, 'r) node -> ('b, 'c, 'r) node -> ('a, 'c, 'r) node
val any : (void, bool, 'r) node -> bool
val chan : in_channel -> ('a, bytes, unit) node
val collect : ('a, 'b, 'c) node -> 'b list
val count : (void, int, 'r) node
val drop : int -> ('a, 'a, 'b) node
val drop_while : ('a -> bool) -> ('a, 'a, 'b) node
val file : string -> (void, string, unit) node
val filter : ('a -> bool) -> ('a, 'a, 'r) node
val fold : init:'a -> f:('a -> 'b -> 'a) -> ('c, 'b, 'd) node -> 'a
val head : (void, 'b, 'r) node -> 'b option
val iota : int -> (void, int, unit) node
val last : (void, 'b, 'r) node -> 'b option
val len : (void, int, 'r) node -> int
val list : 'a list -> (void, 'a, unit) node
val map : ('a -> 'b) -> ('a, 'b, 'r) node
val nth : int -> ('a, 'b, 'r) node -> 'b option
val range : int -> int -> (void, int, unit) node
val repeat : ?n:int -> 'a -> ('b, 'a, unit) node
val slice : int -> int -> ('a, 'a, unit) node
val sum : (void, int, 'r) node -> int
val tail : ('a, 'a, 'b) node
val take : int -> ('a, 'a, unit) node
val take_while : ('a -> bool) -> ('a, 'a, unit) node

