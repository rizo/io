
module type S = sig
  type _ t

  val pure : 'a Lazy.t -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type EXTENSION = sig
  type _ t

  include S with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : 'a t -> 'b t Lazy.t -> 'b t

  val join : 'a t t -> 'a t

  val sequence : 'a t list -> 'a list t
end

module Extend(M : S) : (EXTENSION with type 'a t := 'a M.t) = struct
  include M

  let ( >>= ) m f =
    M.bind f m

  let ( >> ) ma mb =
    ma >>= fun _ -> Lazy.force mb

  let join maa =
    maa >>= fun ma -> ma

  let sequence mas =
    List.fold_right
      (fun ma ms ->
         ma >>= fun a ->
         ms >>= fun s ->
         pure (lazy (a :: s)))
      mas
      (pure (lazy []))
end


