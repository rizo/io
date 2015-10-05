
open Elements

module Trampoline : sig
  type 'a trampoline = unit -> 'a trampoline'
  and 'a trampoline' =
    | Cont of 'a trampoline
    | Stop of 'a
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
    | Stop of 'a

  let pure x = fun () -> Stop x

  let rec (>>=) t f = fun () ->
    match t () with
    | Cont t' -> Cont (t' >>= f)
    | Stop r -> (f r) ()

  let (>>) t1 t2 =
    t1 >>= fun () -> t2

  let pause () = Cont (pure ())

  let rec run t =
    match t () with Cont k -> run k | Stop r -> r

  let rec zip_with f t1 t2 = fun () ->
    match t1 (), t2 () with
    | Cont a, Cont b -> Cont (zip_with f a b)
    | Cont a, Stop b -> Cont (zip_with f a (pure b))
    | Stop a, Cont b -> Cont (zip_with f (pure a) b)
    | Stop a, Stop b -> Stop (f a b)

  let interleave xs =
    List.fold_right (zip_with cons) xs (pure [])
end

module Generator : sig
  type ('a, 'x) gen = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of 'a * ('a, 'x) gen
    | Stop of 'x
  val pure : 'x -> ('a, 'x) gen
  val ( >>= ) : ('a, 'x) gen -> ('x -> ('a, 'y) gen) -> ('a, 'y) gen
  val ( >> ) : ('a, unit) gen -> ('a, 'x) gen -> ('a, 'x) gen
  val yield : 'a -> ('a, unit) gen
  val run : ('a, 'x) gen -> ('a list * 'x)
end = struct
  type ('a, 'x) gen = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of 'a * ('a, 'x) gen
    | Stop of 'x

  let pure x = fun () -> Stop x

  let rec (>>=) g f = fun () ->
    match g () with
    | Cont (a, k) -> Cont (a, k >>= f)
    | Stop x -> (f x) ()

  let (>>) g1 g2 =
    g1 >>= fun () -> g2

  let yield a = fun () -> Cont (a, pure ())

  let rec run g =
    let rec run' f g =
      match g () with
      | Cont (a, k) -> run' Fn.(f @. (cons a)) k
      | Stop x -> (f [], x)
    in run' id g
end


module Iteratee : sig
  type ('a, 'x) iter = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of ('a -> ('a, 'x) iter)
    | Stop of 'x
  val pure : 'x -> ('a, 'x) iter
  val ( >>= ) : ('a, 'x) iter -> ('x -> ('a, 'y) iter) -> ('a, 'y) iter
  val ( >> ) : ('a, unit) iter -> ('a, 'x) iter -> ('a, 'x) iter
  val await : ('a, 'a) iter
  val run : 'a list -> ('a, 'x) iter -> 'x
end = struct
  type ('a, 'x) iter = unit -> ('a, 'x) status'
   and ('a, 'x) status' =
    | Cont of ('a -> ('a, 'x) iter)
    | Stop of 'x

  let pure x = fun () -> Stop x

  let rec (>>=) g f = fun () ->
    match g () with
    | Cont k -> Cont (fun a -> k a >>= f)
    | Stop x -> (f x) ()

  let (>>) g1 g2 =
    g1 >>= fun () -> g2

  let await = fun () -> Cont pure

  let rec run xs i =
    match xs, i () with
    | a::rest, Cont k -> run rest (k a)
    | a::rest, Stop x -> x
    | []     , Cont k -> fail "no more values to feed."
    | []     , Stop x -> x
end

module Coroutine(Suspend : Functor) : sig
  type 'r coroutine = unit -> 'r status
   and 'r status =
    | Cont of 'r coroutine Suspend.t
    | Stop of 'r
  val pure : 'r -> 'r coroutine
  val ( >>= ) : 'a coroutine -> ('a -> 'b coroutine) -> 'b coroutine
  val suspend : 'r coroutine Suspend.t -> 'r coroutine
end = struct
  type 'r coroutine = unit -> 'r status
   and 'r status =
    | Cont of 'r coroutine Suspend.t
    | Stop of 'r

  let pure x = fun () -> Stop x

  let rec (>>=) c f = fun () ->
    match c () with
    | Cont k -> Cont (Suspend.map (fun x -> x >>= f) k)
    | Stop r -> (f r) ()

  let suspend s = fun () -> Cont s
end

module Generalized = struct
  module Trampoline = Coroutine(Id)
  module Generator  = Coroutine(T2)
  module Iteratee   = Coroutine(Fn)
end

module Test_trampoline = struct
  open Trampoline

  let trampoline = begin
    (fun () -> Stop (print_string "hello, ")) >>
    pause >>
    (fun () -> Stop (print "world"))
  end

  let test () =
    match trampoline () with
    | Cont k -> print_string "wonderful "; run k
    | Stop x -> fail "could not bounce"
end

module Test_generator = struct
  open Generator

  let gen = begin
    (fun () -> Stop (print_string "yielding one, ")) >>
    yield 1 >>
    (fun () -> Stop (print_string "then two, ")) >>
    yield 2 >>
    (fun () -> Stop (print_string "returning three: ")) >>
    pure 3
  end

  let test () =
    run gen
end

module Test_iteratee = struct
  open Iteratee

  let iter = begin
    (fun () -> Stop (print_string "enter two numbers: ")) >>
    await >>= fun a ->
    await >>= fun b ->
    (fun () -> Stop (print_endline ("sum is " ^ string_of_int (a + b))))
  end

  let test () =
    run [2; 5] iter
end

