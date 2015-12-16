
open Elements
open IO_core

let (=>) = (>>>)
let (<=) = (<<<)

(* Producers *)

let rec list xs =
  match xs with
  | x::xs' -> yield x >> lazy (list xs')
  | []     -> return ()

let rec chan c =
  match guard input_line c with
  | Some line -> yield line >> lazy (chan c)
  | None      -> return ()

let file file_path =
  let c = open_in file_path in
  chan c >> lazy (return (close_in c))

let rec count =
  let rec loop n =
    yield n >> lazy (loop (n + 1)) in
  loop 0

let rec repeat ?n x =
  match n with
  | Some n -> replicate n (yield x)
  | None   -> forever (yield x)

(* Consumers *)

let rec each f s =
  match next s with
  | Some (a, rest) -> f a; each f rest
  | None           -> ()

let rec fold f acc s =
  match next s with
  | Some (a, s') -> fold f (f acc a) s'
  | None         -> acc

let collect src =
  let rec loop src acc =
    match next src with
    | Some (a, rest) -> loop rest (a::acc)
    | None           -> List.rev acc
  in loop src []

let nth n s =
  if n < 0 then fail "nth: negative index"
  else
    let rec loop n s =
      match next s with
      | Some (a,  _) when n = 0 -> Some a
      | Some (_, s')            -> loop (n - 1) s'
      | None                    -> None
    in loop n s

let head s =
  match next s with
  | Some (a, _) -> Some a
  | None        -> None

let sum s =
  fold (+) 0 s

let length s = fold (fun c _ -> c + 1) 0 s

let rec any s =
  match next s with
  | Some (a, _) when a -> a
  | Some (a, s')       -> any s'
  | None               -> false

let last s =
  let rec loop last_opt s =
    match next s with
    | Some (a, s') -> loop (Some a) s'
    | None         -> last_opt in
  loop None s

let rec map_rec f =
  await >>= fun a -> yield (f a) >> lazy (map_rec f)

let rec map_rec_cons f =
  Await (fun a -> Yield (f a, lazy (map_rec f)))

let map_forever f =
  forever (await >>= fun a -> yield (f a))

let map = map_forever

let rec filter f =
  await >>= fun a ->
  if f a then yield a >> lazy (filter f)
  else filter f

let rec filter_map f =
  await >>= fun a ->
  match f a with
  | Some a' -> yield a' >> lazy (filter_map f)
  | None    -> filter_map f

let rec take_rec n =
  if n < 0 then raise (Invalid_argument "take: negative index")
  else if n = 0 then return ()
  else await >>= fun i -> yield i >> lazy (take_rec (n - 1))

let take_replicate n =
  replicate n (await >>= yield)

let take = take_replicate

let rec take_while pred =
  await >>= fun a ->
  if pred a then yield a >> lazy (take_while pred)
  else return ()

let rec drop n =
  if n = 0 then id
  else await >>= fun a -> drop (n - 1)

let rec drop_while pred =
  await >>= fun a ->
  if pred a then drop_while pred
  else id

let tail = Await (fun _ -> id)

let rec iota stop =
  count => take stop

let range start stop =
  count => take stop => drop start

let slice i j =
  drop i => take (j - i)

module Explicit
  : sig
    val filter : ('b -> bool) -> ('a, 'b, 'r) stream -> ('a, 'b, 'r) stream
    val map : ('b -> 'c) -> ('a, 'b, 'r) stream -> ('a, 'c, 'r) stream
    val take : int -> ('a, 'b, unit) stream -> ('a, 'b, unit) stream
  end
= struct

  let await_from s =
    s >>> await

  let rec map f =
    await >>= fun a ->
      yield (f a) >> lazy (map f)

  let rec map f s =
    await_from s >>= fun a ->
      yield (f a) >> lazy (map f s)

  let rec map f s =
    let a = await_from s in
    yield (f a);

    await_from s >>= fun a ->
      yield (f a) >> lazy (map f s)

  let rec map f s =
    s >>> await >>= fun a ->
      yield (f a) >> lazy (map f s)

  let rec map f s =
    s >>> forever (await >>= fun a -> yield (f a))

  let filter f s =
    s >>> filter f

  let take n s =
    s >>> take n

end


