
type ('i, 'o, 'r) pipe =
    | Value of 'r
    | Yield of ('o *  ('i, 'o, 'r) pipe)
    | Await of ('i -> ('i, 'o, 'r) pipe)

let rec fuse p1 p2 =
  match p2 with
  | Value r -> r
  | Yield (x, p2') -> Yield (x, (fuse p1 p2'))
  | Await k ->
    let rec go = function
      | Value r -> Value r
      | Yield (x, p2') -> fuse p2' (k x)
      | Await k' -> Await (fun x -> go (k' x))
    in
      go p1

let (>=>) = fuse

let rec id = Await (fun x -> Yield (x, id))

let consume =
  let rec step acc count =
    if count = 0
    then Value (List.rev acc)
    else Await (fun i -> step (i::acc) (count - 1))
  in
    step []

let rec yield_many = function
  | [] -> failwith "FIXME"
  | o::os -> Yield (o, (yield_many os))

let rec run_pipe = function
  | Value r -> r
  | Await k -> run_pipe (k ())
  | Yield ((), k) -> run_pipe k

let main () =
  run_pipe (yield_many [1; 2; 3; 4; 5] >=> id)

