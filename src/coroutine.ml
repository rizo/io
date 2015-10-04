
open Elements

module Trampoline : sig
  type 'a t = {
    bounce : ('a t, 'a) either lazy_t
  }
  val pure : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >> ) : unit t -> 'a t -> 'a t
  val pause : unit t
  val run : 'a t -> 'a
end = struct
  type 'a t = {
    bounce : ('a t, 'a) either lazy_t
  }

  let pure x = { bounce = lazy (Right x) }

  let rec (>>=) t f =
    { bounce = match force t.bounce with
      | Left t' -> lazy (Left (t' >>= f))
      | Right r -> (f r).bounce }

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let pause = { bounce = lazy (Left (pure ())) }

  let rec run t =
    force t.bounce |> either run id
end


