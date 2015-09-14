
module type Gen = sig
  type 'a gen

  val return  : 'a -> 'a gen
  val (>>=)   : 'a gen -> ('a -> 'b gen) -> 'b gen
  val (>>)    : unit gen -> 'a gen Lazy.t -> 'a gen

  val forever : unit gen -> unit
  val map : ('a -> 'b) -> 'a gen -> 'b gen
  val yes : string gen

  val input : string gen
  val print : string gen -> unit
end

module Gen : Gen = struct
  type 'a gen = unit -> 'a option

  let return x = fun () -> Some x
  let (>>=) m f = f (m ())
  let (>>) ma mb = ma >>= fun _ -> Lazy.force mb

  let rec forever x = x >> lazy (forever x)
  let map f g = forever (fun () -> f (g ()))
  let yes = return "yes"
  let input = fun () -> Some (input_line stdin)
  let print = map print_endline
  let of_list l =
    fun () ->
      match l with
      | [] -> None
      | x::xs -> Some x
end

open Gen

