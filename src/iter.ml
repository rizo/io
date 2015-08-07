
type 'a input =
  | Item of 'a
  | Empty
  | End

type ('a, 'b) t =
  | Yield    of 'b * 'a input
  | Continue of ('a input -> ('a, 'b) t)
  | Error    of exn

type ('a, 'b) iter = ('a, 'b) t

type ('a, 'b) enum = ('a, 'b) iter -> ('a, 'b) iter

let run step =
  match step with
  | Yield (x, _) -> Some x
  | Continue k ->
    begin match (k End) with
      | Yield (x, _) -> Some x
      | _ -> None
    end
  | Error _ -> None

let run_exn step =
  match step with
  | Yield (x, _) -> x
  | Continue k ->
    begin match (k End) with
      | Yield (x, _) -> x
      | _ -> raise End_of_file
    end
  | Error e -> raise e

let head =
  let rec step = function
    | Item x -> Yield (x, Empty)
    | Empty  -> Continue step
    | End    -> Error End_of_file in
  Continue step

let rec drop n =
  let rec step input =
    match input with
    | Item _ -> drop (n - 1)
    | Empty  -> Continue step
    | End    -> Yield ((), End) in
  if n = 0
    then Yield ((), Empty)
    else Continue step

let length =
  let rec step n = function
    | Item x -> Continue (step (n + 1))
    | Empty  -> Continue (step n)
    | End    -> Yield (n, End) in
  Continue (fun i -> step 0 i)

let last =
  let rec step r = function
    | Item x -> Continue (step (Some x))
    | Empty  -> Continue (step r)
    | End    -> Yield (r, End) in
  Continue (fun i -> step None i)

let pure x =
  Yield (x, Empty)

let rec bind m f =
  match m with
  | Yield (x, input) ->
    begin match f x with
      | Yield (x', _) -> Yield (x', input)
      | Continue k    -> k input
      | Error e       -> raise e
    end
  | Continue k -> Continue (fun input -> bind (k input) f)
  | Error e -> raise e

let (>>=) = bind

let seq m k = m >>= fun _ -> k

let (>>) = seq

let rec map f =
  let rec step acc input =
    match input with
    | Item x -> Continue (step (f x::acc))
    | Empty  -> Continue (step acc)
    | End    -> Yield (acc, End) in
  Continue (step [])

let rec filter f =
  let rec step acc = function
    | Item x when f x -> Continue (step (x::acc))
    | Item _ | Empty  -> Continue (step acc)
    | End             -> Yield (acc, End) in
  Continue (step [])

let rec fmap f = function
  | Yield (x, input) -> Yield (f x, input)
  | Continue k -> Continue (fun input -> fmap f (k input))
  | Error e -> raise e

let rec enum_list xs step =
  match (step, xs) with
  | Continue k, x::xs' -> enum_list xs' (k (Item x))
  | _ -> step

