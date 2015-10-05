
(*
 * Elements
 *)

module type Type0 = sig
  type t
end

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
  type 'a t = 'a
  let map x = x
end

module Fn = struct
  type ('a, 'b) t = 'a -> 'b
  let compose f g = fun x -> f (g x)
  let apply f x = f x
  let map f x = compose f x
  let id x = x
  let flip f x y = f y x
  let (@@) = apply
  let (@.) = compose
end

module T2 = struct
  type ('a, 'b) t = ('a * 'b)
  let map f (x, y) = (x, f y)
end

module Opt = struct
  type 'a t = 'a option

  exception No_value

  let value_exn opt =
    match opt with
    | Some x -> x
    | None -> raise No_value

  let value ~default opt =
    match opt with
    | Some x -> x
    | None -> default

  (*
   * Monad Implementation
   *)

  let return x =
    Some x

  let bind opt f =
    match opt with
    | Some x -> f x
    | None -> None

  let zero =
    return ()

  let delay f =
    f

  let combine opt dopt =
    bind opt (fun () -> dopt ())

  let run dopt = dopt ()

  let (>>=) = bind
  let (>>)  = combine

  (*
   * Monadic Combinators
   *)

  let rec forever opt =
    opt >> fun () -> forever opt

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

  include Either.Public

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

  (* Fn *)
  let flip = Fn.flip
  let id = Fn.id

  (* List *)
  let cons x xs = x::xs
end

include Base

