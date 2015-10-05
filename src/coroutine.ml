
open Elements

module Trampoline : sig
  type 'a trampoline = unit -> 'a trampoline'
  and 'a trampoline' =
    | Cont of 'a trampoline
    | Done of 'a
  type 'a t = 'a trampoline
  val pure : 'a -> 'a trampoline
  val ( >>= ) : 'a trampoline -> ('a -> 'b trampoline) -> 'b trampoline
  val ( >> ) : unit trampoline -> 'a trampoline -> 'a trampoline
  val pause : unit trampoline
  val run : 'a trampoline -> 'a
  val zip_with : ('a -> 'b -> 'c) -> 'a trampoline -> 'b trampoline -> 'c trampoline
  val interleave : 'a trampoline list -> 'a list trampoline
end = struct
  type 'a trampoline = unit -> 'a trampoline'
  and 'a trampoline' =
    | Cont of 'a trampoline
    | Done of 'a

  type 'a t = 'a trampoline

  let pure x = fun () -> Done x

  let rec (>>=) t f = fun () ->
    match t () with
    | Cont t' -> Cont (t' >>= f)
    | Done r -> (f r) ()

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let pause () = Cont (pure ())

  let rec run t =
    match t () with Cont k -> run k | Done r -> r

  let rec zip_with f t1 t2 = fun () ->
    match t1 (), t2 () with
    | Cont a, Cont b -> Cont (zip_with f a b)
    | Cont a, Done b -> Cont (zip_with f a (pure b))
    | Done a, Cont b -> Cont (zip_with f (pure a) b)
    | Done a, Done b -> Done (f a b)

  let interleave xs =
    List.fold_right (zip_with cons) xs (pure [])
end

module Test = struct
  open Trampoline

  let hello name = begin
    (fun () -> Done (print_string "hello, ")) >>
    pause >>
    (fun () -> Done (print name))
  end

  let test_hello () =
    match (hello "world") () with
    | Cont k -> print_string "wonderful "; run k
    | Done x -> fail "could not bounce"
end

