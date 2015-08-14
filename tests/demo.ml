
open Iter

let sum_iter =
  let rec step total = function
    | Input.Item x -> Await (step (total + x))
    | Input.Empty  -> Await (step total)
    | Input.End    -> Yield (total, Input.End) in
  Await (step 0)

let reverse =
  let rec step acc = function
    | Input.Item x -> Await (fun i -> step (x::acc) i)
    | Input.Empty  -> Await (fun i -> step acc i)
    | Input.End    -> Yield (acc, Input.End) in
  Await (fun i -> step [] i)

let () =
  let res = run_exn (enum_list [1; 2; 3; 4; 5] >=> map_enum ((+) 1)) in
  List.iter (fun x -> print_endline (string_of_int x)) res

