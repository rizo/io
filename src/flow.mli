
(*


   Generator
   Processor


   Upstream | Downstream


        +-------------+
        |             |
 a_out <--           <-- b_in
        |    node0    |
 a_in  -->           --> b_out
        |             |
        +-------------+



## Early termination by consumers

Here is an example of a sink with early termination handling.

  let rec any flow =
    match next flow with
    | Some (a, _) when a -> a
    | Some (a, flow) -> any flow
    | None -> false

Any requests the `next` value from the flow until it gets a truthy value,
after this condition is satisfied the flow is suspended...


 *)


open Elements

(* flow represents a state of continuous computation. *)
type ('a, 'b, 'r) flow = ('a, 'b, 'r) node Lazy.t
 and ('a, 'b, 'r) node =
   | Ready of 'r
   | Yield of ('b  * ('a, 'b, 'r) flow)
   | Await of ('a -> ('a, 'b, 'r) flow)
type ('a, 'b, 'r) t = ('a, 'b, 'r) flow

(* Source is a flow that can only `yield`. *)
type ('b, 'r) source = (void, 'b, 'r) flow

(* Processor applies a transformation to each element of the flow. *)
type ('a, 'b) processor = ('a,   'b, void) flow

(* Sink is a flow that can only `await` and `return` a result when done. *)
type ('a, 'r)      sink = ('a, void,   'r) flow

val return : 'r -> ('a, 'b, 'r) flow

val ( >>= ) : ('a, 'b, 'r) flow -> ('r -> ('a, 'b, 's) flow) -> ('a, 'b, 's) flow

val ( >> )  : ('a, 'b, 'r) flow -> ('a, 'b, 's) flow -> ('a, 'b, 's) flow

val yield : 'a -> ('b, 'a, unit) flow

val await : ('a, 'b, 'a) flow

val compose : ('a, 'b, 'r) flow -> ('d, 'a, 'r) flow -> ('d, 'b, 'r) flow
val ( <= ) : ('a, 'b, 'r) flow -> ('d, 'a, 'r) flow -> ('d, 'b, 'r) flow
val ( => ) : ('a, 'b, 'r) flow -> ('b, 'd, 'r) flow -> ('a, 'd, 'r) flow

val cat : ('a, 'a, void) flow

val run : (void, void, 'r) flow -> 'r

val next : ('a, 'r) source -> ('a * ('a, 'r) source) option

val get_line_from_chan : in_channel -> (string, 'r) source

val get_line_from_file : string -> (string, 'r) source

val get_char_from_chan : in_channel -> (char, 'r) source

val of_list : 'a list -> ('a, 'r) source

val collect : ('a, 'r) source -> 'a list

val len : ('a, 'r) source -> int

val discard : ('a, unit) sink

val drop : int -> ('a, 'a) processor

val filter : ('a -> bool) -> ('a, 'a, 'b) flow

val fold : ('a -> 'b -> 'a) -> 'a -> ('b, 'r) source -> 'a

val head : ('a, 'r) source -> ('a, exn) result

val count : (int, 'r) source

val iota : int -> (int, 'r) source

val last : ('a, 'r) source -> 'a option

val map : ('a -> 'b) -> ('a, 'b) processor

val nth : int -> ('a, 'b, 'a option) flow

val put_line : (string, 'r) source -> unit

val range : int -> int -> (int, 'r) source

val repeat : 'a -> ('a, 'r) source

val sum : (int, 'r) source -> int

val tail : ('a, 'a, 'r option) flow

val take : int -> ('a, 'a, 'r option) flow

val yes : (string, 'r) source

val any : (bool, 'r) source -> bool

