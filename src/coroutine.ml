
open Elements

module Trampoline : sig
  type 'a trampoline = unit -> 'a trampoline'
   and 'a trampoline' = {
    bounce : ('a trampoline, 'a) either
  }
  type 'a t = 'a trampoline
  val bounce : 'a trampoline -> ('a trampoline, 'a) either
  val pure : 'a -> 'a trampoline
  val ( >>= ) : 'a trampoline -> ('a -> 'b trampoline) -> 'b trampoline
  val ( >> ) : unit trampoline -> 'a trampoline -> 'a trampoline
  val lift : 'a -> 'a trampoline'
  val pause : unit trampoline
  val run : 'a trampoline -> 'a
  val zip_with : ('a -> 'b -> 'c) -> 'a trampoline -> 'b trampoline -> 'c trampoline
  val interleave : 'a trampoline list -> 'a list trampoline
end = struct
  type 'a trampoline = unit -> 'a trampoline'
  and 'a trampoline' = {
    bounce : ('a trampoline, 'a) either
  }
  type 'a t = 'a trampoline

  let bounce t = (t ()).bounce

  let pure x = fun () -> { bounce = Right x }

  let rec (>>=) t f = fun () ->
    { bounce = match bounce t with
        | Left t' -> Left (t' >>= f)
        | Right r -> bounce (f r) }

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let lift r = { bounce = Right r }

  let pause () = { bounce = Left (pure ()) }

  let rec run t =
    bounce t |> either run id

  let rec zip_with f t1 t2 = fun () ->
    { bounce = match bounce t1, bounce t2 with
        | Left  a, Left  b -> Left (zip_with f a b)
        | Left  a, Right b -> Left (zip_with f a (pure b))
        | Right a, Left  b -> Left (zip_with f (pure a) b)
        | Right a, Right b -> Right (f a b) }

  let interleave xs = List.fold_right (zip_with cons) xs (pure [])
end

module Test = struct
  open Trampoline

  let hello name = begin
    (fun () -> lift (print_string "hello, ")) >>
    pause >>
    (fun () -> lift (print name))
  end

  let test_hello () =
    match bounce (hello "world") with
    | Left k -> print_string "wonderful "; run k
    | Right x -> fail "could not bounce"
end

