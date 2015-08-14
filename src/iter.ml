

let fmt = Printf.sprintf
let print str = print_endline str

module Input = struct
  type 'a t =
    | Item of 'a
    | Empty
    | End

  let show show_item input =
    match input with
    | Item x -> "Item " ^ show_item x
    | Empty -> "Empty"
    | End -> "End"

  let map f input =
    match input with
    | Item x -> Item (f x)
    | _ -> input
end

type 'a input = 'a Input.t

type ('a, 'b) iter =
  | Yield of 'b * 'a input
  | Await of ('a input -> ('a, 'b) iter)
  | Error of exn

type ('a, 'b) enum = ('a, 'b) iter -> ('a, 'b) iter

let run step =
  match step with
  | Yield (x, _) -> Some x
  | Await k ->
    begin match (k Input.End) with
      | Yield (x, _) -> Some x
      | _ -> None
    end
  | Error _ -> None

let run_exn step =
  match step with
  | Yield (x, _) -> x
  | Await k ->
    begin match (k Input.End) with
      | Yield (x, _) -> x
      | Await _ -> raise (Failure "run_exn: divergent iteratee")
      | Error e -> raise e
    end
  | Error e -> raise e

let pure x =
  Yield (x, Input.Empty)

let rec bind m f =
  match m with
  | Yield (x, input) ->
    begin match f x with
      | Yield (x', _) -> Yield (x', input)
      | Await k    -> k input
      | Error e    -> Error e
    end
  | Await k -> Await (fun input -> bind (k input) f)
  | Error e -> Error e

let bind' iter f =
  let rec go = function
    | Await k          -> Await (fun input -> go (k input))
    | Yield (r, input) -> Yield (f r, input)
    | Error e          -> Error e
  in
  go iter


let (>>=) = bind

let seq m k = m >>= fun _ -> k

let (>>) = seq

let (>=>) e1 e2 : ('a, 'b) enum =
  fun x -> e2 (e1 x)

let (=>) f x = f x

let rec fmap f = function
  | Yield (x, input) -> Yield (f x, input)
  | Await k -> Await (fun input -> fmap f (k input))
  | Error e -> raise e

(* Combinators *)

let head =
  let rec step = function
    | Input.Item x -> Yield (x, Input.Empty)
    | Input.Empty  -> Await step
    | Input.End    -> Error End_of_file in
  Await step

let rec drop n =
  let rec step input =
    match input with
    | Input.Item _ -> drop (n - 1)
    | Input.Empty  -> Await step
    | Input.End    -> Yield ((), Input.End) in
  if n = 0
  then Yield ((), Input.Empty)
  else Await step

let length =
  let rec step n = function
    | Input.Item x -> Await (step (n + 1))
    | Input.Empty  -> Await (step n)
    | Input.End    -> Yield (n, Input.End) in
  Await (fun i -> step 0 i)

let last =
  let rec step r = function
    | Input.Item x -> Await (step (Some x))
    | Input.Empty  -> Await (step r)
    | Input.End    -> Yield (r, Input.End) in
  Await (fun i -> step None i)

let sum =
  let rec step total = function
    | Input.Item x -> Await (step (total + x))
    | Input.Empty  -> Await (step total)
    | Input.End    -> Yield (total, Input.End) in
  Await (step 0)

let to_list =
  let rec step acc = function
    | Input.Item x -> Await (fun input -> step (x::acc) input)
    | Input.Empty  -> Await (fun input -> step acc input)
    | Input.End    -> Yield (List.rev acc, Input.End) in
  Await (fun input -> step [] input)

let rec map' f step =
  match step with
  | Yield (r, _) -> Yield (r, Input.End)
  | Await k -> Await (fun input -> map' f (k (Input.map f input)))
  | Error e -> Error e

let await_iter : (int, int) iter =
  let rec step = function
    | Input.Item x -> print (fmt "await: item: %d" x); Yield (x, Input.Empty)
    | Input.Empty  -> print "await: empty"; Await (fun input -> step input)
    | Input.End    -> print "await: end"; Error End_of_file in
  Await (fun input -> step input)

let rec await iter =
  match iter with
  | Yield (r, _) -> Yield (r, Input.End)
  | Await k -> Await (fun input -> await (k input))
  | _ -> iter

let yield x =
  Yield (x, Input.Empty)

let rec forever x = x >> forever x

(* type Enumeratee o i r = Iter i r -> Iter o (Iter i r) *)
(* map :: (o -> i) -> Enumeratee o i b                   *)
(* map :: (o -> i) -> Iter i r -> Iter o (Iter i r)      *)

(* let rec map : ('o -> 'i) -> ('i, 'r) iter -> ('o, ('i, 'r) iter) iter = *)
(*   fun f iter ->                                                         *)
(*     match iter with                                                     *)
(*     | Yield (r, _) -> Yield (r, Input.End)                              *)
(*     | Await k -> Await (fun input -> map f (k (Input.map f input)))     *)
(*     | Error e -> Error e                                                *)

let rec map_await f iter : ('a, 'r) iter =
  await iter >>= fun x -> yield (f x) >> map_await f iter

let rec map_list f xs =
  match xs with
  | [] -> []
  | x::xs' -> f x :: map_list f xs'

let rec map_enum f step =
  match step with
  | Yield (r, _) -> Yield (r, Input.End)
  | Await k -> Await (fun input -> map_enum f (k (Input.map f input)))
  | Error e -> Error e

(* let rec map_eteratee f iter =                                                    *)
(*   let rec step (k : 'd input -> ('a, ('b, 'c) iter) iter) input =                *)
(*     match input with                                                             *)
(*     | Input.Empty -> Await (fun input -> step k input)                           *)
(*     | Input.Item x -> k (Input.map f input) >>= fun x -> pure (map_eteratee f x) *)
(*     | Input.End -> Yield ((Await k), input) in                                   *)
(*   match iter with                                                                *)
(*   | Await k -> Await (fun input -> step k input)                                 *)
(*   | _ -> pure iter                                                               *)




(* Enumerators *)

let rec enum_eof iter =
  match iter with
  | Yield (x, _) -> Yield (x, Input.End)
  | Await k ->
    begin match k Input.End with
      | Await _ -> raise (Failure "enum_eof: divergent iteratee")
      | i -> enum_eof i
    end
  | Error e -> Error e

let rec enum_list xs iter =
  match (iter, xs) with
  | Await k, x::xs' -> enum_list xs' (k (Input.Item x))
  | _-> iter

let check_done f iter =
  match iter with
  | Yield _ -> iter
  | Await k -> f k
  | Error e -> raise e

