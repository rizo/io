
open Elements

module Trampoline : sig
  type 'a trampoline = unit -> 'a trampoline'
  and 'a trampoline' =
    | Cont of 'a trampoline
    | Done of 'a
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

module Generator : sig
  type ('a, 'x) gen = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of 'a * ('a, 'x) gen
    | Done of 'x
  val pure : 'x -> ('a, 'x) gen
  val ( >>= ) : ('a, 'x) gen -> ('x -> ('a, 'y) gen) -> ('a, 'y) gen
  val ( >> ) : ('a, unit) gen -> ('a, 'x) gen -> ('a, 'x) gen
  val yield : 'a -> ('a, unit) gen
  val run : ('a, 'x) gen -> ('a list * 'x)
end = struct
  type ('a, 'x) gen = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of 'a * ('a, 'x) gen
    | Done of 'x

  let pure x = fun () -> Done x

  let rec (>>=) g f = fun () ->
    match g () with
    | Cont (a, k) -> Cont (a, k >>= f)
    | Done x -> (f x) ()

  let (>>) g1 g2 =
    g1 >>= fun () -> g2

  let yield a = fun () -> Cont (a, pure ())

  let rec run g =
    let rec run' f g =
      match g () with
      | Cont (a, k) -> run' Fn.(f @. (cons a)) k
      | Done x -> (f [], x)
    in run' id g
end


module Iteratee : sig
  type ('a, 'x) iter = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of ('a -> ('a, 'x) iter)
    | Done of 'x
  val pure : 'x -> ('a, 'x) iter
  val ( >>= ) : ('a, 'x) iter -> ('x -> ('a, 'y) iter) -> ('a, 'y) iter
  val ( >> ) : ('a, unit) iter -> ('a, 'x) iter -> ('a, 'x) iter
  val await : ('a, 'a) iter
  val run : 'a list -> ('a, 'x) iter -> 'x
end = struct
  type ('a, 'x) iter = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of ('a -> ('a, 'x) iter)
    | Done of 'x

  let pure x = fun () -> Done x

  let rec (>>=) g f = fun () ->
    match g () with
    | Cont k -> Cont (fun a -> k a >>= f)
    | Done x -> (f x) ()

  let (>>) g1 g2 =
    g1 >>= fun () -> g2

  let await = fun () -> Cont pure

  let rec run xs i =
    match xs, i () with
    | a::rest, Cont k -> run rest (k a)
    | a::rest, Done x -> x
    | []     , Cont k -> fail "no more values to feed."
    | []     , Done x -> x
end

module Test_trampoline = struct
  open Trampoline

  let trampoline = begin
    (fun () -> Done (print_string "hello, ")) >>
    pause >>
    (fun () -> Done (print "world"))
  end

  let test () =
    match trampoline () with
    | Cont k -> print_string "wonderful "; run k
    | Done x -> fail "could not bounce"
end

module Test_generator = struct
  open Generator

  let gen = begin
    (fun () -> Done (print_string "yielding one, ")) >>
    yield 1 >>
    (fun () -> Done (print_string "then two, ")) >>
    yield 2 >>
    (fun () -> Done (print_string "returning three: ")) >>
    pure 3
  end

  let test () =
    run gen
end

module Test_iteratee = struct
  open Iteratee

  let iter = begin
    (fun () -> Done (print_string "enter two numbers: ")) >>
    await >>= fun a ->
    await >>= fun b ->
    (fun () -> Done (print_endline ("sum is " ^ string_of_int (a + b))))
  end

  let test () =
    run [2; 5] iter
end

