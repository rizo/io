
open Iter

let sum_iter =
  let rec step total = function
    | Item x -> Continue (step (total + x))
    | Empty  -> Continue (step total)
    | End    -> Yield (total, End) in
  Continue (step 0)

let reverse =
  let rec step acc = function
    | Item x -> Continue (fun i -> step (x::acc) i)
    | Empty  -> Continue (fun i -> step acc i)
    | End    -> Yield (acc, End) in
  Continue (fun i -> step [] i)

let test () =
  let job = drop 1 >> (map ((+) 1)) in
  let res = run_exn (enum_list [1; 2; 3; 4; 5] job) in
  List.iter (fun x -> print_endline (string_of_int x)) res

let () =
  test ()

