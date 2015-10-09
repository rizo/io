
(*
 * Elements
 *)

module type Type0 = sig
  type t
end

module type Type = Type0

module type Type1 = sig
  type 'a t
end

module type Type2 = sig
  type ('a, 'b) t
end

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end


module Id = struct
  module Make (X : Type) = X

  type 'a t = 'a
  let map f x = f x
end

module Fn = struct
  type ('a, 'b) t = 'a -> 'b
  let compose f g = fun x -> f (g x)
  let invcompose g f = fun x -> f (g x)
  let apply f x = f x
  let map f x = compose f x
  let id x = x
  let flip f x y = f y x
  let (@@) = apply
  let (<<) = compose
  let (>>) = invcompose

  module Public = struct
    let (<<) = (<<)
    let (>>) = (>>)
    let id = id
    let flip = flip
  end
end

module T2 = struct
  type ('a, 'b) t = ('a * 'b)
  let map f (x, y) = (x, f y)
end

module Option = struct
  type 'a t = 'a option

  exception No_value

  let some x = Some x

  let option if_none if_some opt =
    match opt with
    | None -> if_none
    | Some a -> if_some a

  module Public = struct
    let some = some
    let option = option
  end

  let value_exn opt =
    match opt with
    | Some x -> x
    | None -> raise No_value

  let value ~default opt =
    match opt with
    | Some x -> x
    | None -> default

  let return x = Some x

  let (>>=) opt f =
    match opt with
    | Some x -> f x
    | None -> None

  let (>>|) opt f =
    match opt with
    | Some x -> Some (f x)
    | None -> None

  let (>>) opt1 opt2 =
    opt1 >>= fun _ -> opt2
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Exn = struct
  let as_option e f x =
    try Some (f x)
    with e' when e' = e -> None

  let fail msg = raise (Failure msg)
end


module type Coroutine = sig
  type ('i, 'o) t
  val map : ('a -> 'b) -> ('i, 'a) t -> ('i, 'b) t
end

module Coroutine : Coroutine = struct
  type ('i, 'o) t = {
    run : ('i -> ('o * ('i, 'o) t))
  }

  let rec map f c = {
    run = fun i ->
      let (o, c') = c.run i in
      (f o, map f c')
  }
end

module List = struct
  let rec range s e =
    if s = e then []
    else s::range (s + 1) e

  let iota = range 0

  let rev xs =
    let rec loop acc xs =
      match xs with
      | [] -> acc
      | x::xs' -> loop (x::acc) xs'
    in
    loop [] xs
end

module Either = struct
  module Public = struct
    type ('a, 'b) either =
      | Left  of 'a
      | Right of 'b

    let either f g x =
      match x with
      | Left  l -> f l
      | Right r -> g r
  end
  include Public
  type ('a, 'b) t = ('a, 'b) either

  let pure x = Right x

  let (>>=) m f =
    match m with
    | Right x -> f x
    | Left e  -> Left e
end

module Base = struct
  type void = Void

  type ('a, 'e) result =
    | Ok    of 'a
    | Error of 'e

  let discard _ = ()

  include Either.Public
  include Option.Public
  include Fn.Public

  (* Lazy *)
  let force = Lazy.force

  let time f x =
    let t = Unix.gettimeofday () in
    let fx = f x in
    Printf.printf "Elapsed time: %f sec\n"
      (Unix.gettimeofday () -. t);
    fx

  let fail = Exn.fail

  let print = print_endline
  let fmt = Printf.sprintf

  (* List *)
  let cons x xs = x::xs

  let even n = n mod 2 = 0
  let odd  n = n mod 2 = 1
end

include Base

