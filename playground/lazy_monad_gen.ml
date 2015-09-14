
open Elements

type 'a step =
  | Stop
  | Next of 'a * 'a gen
 and 'a gen = 'a step lazy_t

let return x =
  lazy (Next (x, lazy Stop))

(* val bind : 'a gen -> ('b -> 'b gen) -> 'b gen *)
let rec (>>=) m f =
  match Lazy.force m with
  | Stop -> lazy Stop
  | Next (x, m') -> f x

let rec (>>|) m f =
  m >>= fun x -> return (f x)

let (>>) m1 m2 =
  m1 >>= fun () -> m2

let rec forever g = g >> forever g

let map_forever f g = forever (g >>= fun x -> return (f x))

let rec map f g =
  lazy begin match Lazy.force g with
    | Stop -> Stop
    | Next (x, g') -> Next (f x, map f g')
  end

let rec fold f z g =
  match Lazy.force g with
  | Stop -> z
  | Next (x, g') -> fold f (f z x) g'

let rec of_list l : 'a gen =
  lazy begin match l with
    | []    -> Stop
    | x::xs -> Next (x, of_list xs)
  end

let rec of_chan ch : string gen =
  lazy begin
    match Exn.as_option End_of_file input_line ch with
    | None   -> Stop
    | Some l -> Next (l, of_chan ch)
  end

let rec of_file filename : string gen =
  let ch = open_in filename in
  lazy begin
    match Exn.as_option End_of_file input_line ch with
    | None   -> close_in ch; Stop
    | Some l -> Next (l, of_chan ch)
  end

let rec to_list gen =
  match Lazy.force gen with
  | Stop -> []
  | Next (x, xs) -> x :: to_list xs

let len =
  fold (fun n _ -> n + 1) 0

let rec take n gen =
  lazy begin
    if n = 0 then Stop
    else match Lazy.force gen with
      | Stop -> Stop
      | Next (x, xs) -> Next (x, (take (n - 1) xs))
  end

let rec yes =
  lazy (Next ("y", yes))

let count () =
  let rec loop n =
    lazy (Next (n, loop (n + 1))) in
  loop 0

(* let count () =                 *)
(*   let rec loop n =             *)
(*     yield n >> loop (n + 1) in *)
(*   loop 0                       *)

