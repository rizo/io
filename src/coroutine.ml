
open Elements

module Trampoline : sig
  type 'a t = unit -> 'a s
   and 'a s = {
    bounce : ('a t, 'a) either
  }
  val pure : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : unit t -> 'a t -> 'a t
  val pause : unit t
  val run : 'a t -> 'a
end = struct
  type 'a t = unit -> 'a s
   and 'a s = {
    bounce : ('a t, 'a) either
  }

  let pure x = fun () -> { bounce = Right x }

  let rec (>>=) t f = fun () ->
    { bounce = match (t ()).bounce with
      | Left t' -> Left (t' >>= f)
      | Right r -> (f r ()).bounce }

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let pause () = { bounce = Left (pure ()) }

  let rec run t =
    (t ()).bounce |> either run id
end


