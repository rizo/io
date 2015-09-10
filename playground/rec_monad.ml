
module type Gen = sig
  type 'a t

  val return  : 'a -> 'a t
  val (>>=)   : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)    : unit t -> 'a t Lazy.t -> 'a t

  val forever : unit t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val yes : string t

  val input : string t
  val print : string t -> unit
end

module Gen : Gen = struct
  type 'a t = unit -> 'a

  let return x = fun () -> x
  let (>>=) m f = f (m ())
  let (>>) ma mb = ma >>= fun _ -> Lazy.force mb

  let rec forever x = x >> lazy (forever x)
  let map f g = forever (fun () -> f (g ()))
  let yes = return "yes"
  let input = fun () -> input_line stdin
  let print = map print_endline
end

module G = Gen

