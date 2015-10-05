
open Elements

module Trampoline : sig
  type 'a trampoline = unit -> 'a trampoline'
   and 'a trampoline' = {
    bounce : ('a trampoline, 'a) either
  }
  val pure : 'a -> 'a trampoline
  val ( >>= ) : 'a trampoline -> ('a -> 'b trampoline) -> 'b trampoline
  val ( >> ) : unit trampoline -> 'a trampoline -> 'a trampoline
  val lift : 'a -> 'a trampoline'
  val pause : unit trampoline
  val run : 'a trampoline -> 'a
end = struct
  type 'a trampoline = unit -> 'a trampoline'
   and 'a trampoline' = {
    bounce : ('a trampoline, 'a) either
  }

  let pure x = fun () -> { bounce = Right x }

  let rec (>>=) t f = fun () ->
    { bounce = match (t ()).bounce with
      | Left t' -> Left (t' >>= f)
      | Right r -> (f r ()).bounce }

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let lift r = { bounce = Right r }

  let pause () = { bounce = Left (pure ()) }

  let rec run t =
    (t ()).bounce |> either run id

end

module Test = struct
  open Trampoline

  let hello = begin
    (fun () -> lift (print_string "hello, ")) >>
    pause >>
    (fun () -> lift (print "world!"))
  end

  let test_hello () =
    match (hello ()).bounce with
    | Left k -> print_string "wonderful "; run k
    | Right x -> fail "could not bounce"
end

