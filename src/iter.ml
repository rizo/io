
type 'a input =
  | Item of 'a
  | Empty
  | End

type ('a, 'b) iter =
  | Done of 'b * 'a input
  | Next of ('a input -> ('a, 'b) iter)

type ('a, 'b) t = ('a, 'b) iter

let rec enum it xs =
  match (it, xs) with
  | (it, [])       -> it
  | (Done _, _)    -> it
  | (Next k, h::t) -> enum (k (Item h)) t

let run it =
  match it with
  | Done (x, _) -> Some x
  | Next k -> begin match (k End) with
      | Done (x, _) -> Some x
      | _ -> None
    end

let head =
  let rec step input =
    match input with
    | Item x -> Done (Some x, Empty)
    | Empty  -> Next step
    | End    -> Done (None, End) in
  Next step

let rec drop n =
  let rec step input =
    match input with
    | Item _ -> drop (n - 1)
    | Empty  -> Next step
    | End    -> Done ((), End) in
  if n = 0
    then Done ((), Empty)
    else Next step

let length =
  let rec step acc input =
    match input with
    | Item _ -> Next (step (acc + 1))
    | Empty  -> Next (step acc)
    | End    -> Done (acc, End) in
  Next (step 0)

let pure x =
  Done (x, Empty)

let rec bind m f =
  match m with
  | Done (x, input) ->
    begin match f x with
      | Done (x', _) -> Done (x', input)
      | Next k       -> k input
    end
  | Next k -> Next (fun input -> bind (k input) f)

let (>>=) = bind

let seq m k = m >>= fun _ -> k

let (>>) = seq

let rec map f = function
  | Done (x, input) -> Done (f x, input)
  | Next k -> Next (fun input -> map f (k input))

