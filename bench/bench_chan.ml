
open Elements
open IO.Core
open IO.Iter

(* 0 *)
let stream_fold f stream init =
  let result = ref init in
  Stream.iter (fun x -> result := f x !result) stream;
  !result
let chan_stream input =
  stream_fold (fun i c -> c + 1)
    (Stream.from (fun i -> guard input_line input)) 0

(* 1 *)
let rec chan_try_out input =
  let rec loop () =
    yield (input_line input) >> lazy (loop ()) in
  try loop ()
  with End_of_file -> return ()

(* 2 *)
let rec chan_try_in input =
  let rec loop () =
    try yield (input_line input) >> lazy (loop ())
    with End_of_file -> return () in
  loop ()

(* 3 *)
let rec chan_guard input =
  let rec loop () =
    match guard input_line input with
    | Some line -> yield line >> lazy (loop ())
    | None -> return () in
  loop ()

(* 4 *)
let rec chan_guard_inline input =
  let rec loop () =
    let line_opt =
      try Some (input_line input)
      with End_of_file -> None in
    match line_opt with
    | Some l -> yield l >> lazy (loop ())
    | None -> return () in
  loop ()

(* 5 *)
let rec chan_evil input =
  let rec loop term input =
    let line_with_term =
      try input_line input
      with End_of_file -> term in
    if line_with_term == term then return ()
    else yield line_with_term >> lazy (loop term input) in
  loop "\n" input

let run name chan_fn file_path =
  let input = open_in file_path in
  print ("=> " ^ name);
  print (fmt " # %d" (length (chan_fn input)))

let run' name chan_fn file_path =
  let input = open_in file_path in
  print ("=> " ^ name);
  print (fmt " # %d" (chan_fn input))

let () =
  let file_path =
    match Sys.argv with
    | [|_; file_path|] -> file_path
    | _ ->
      print "usage: bench_chan <file_path>";
      exit 0 in
  begin
    ignore (time (run' "chan_stream"       chan_stream)       file_path);
    ignore (time (run "chan_evil"         chan_evil)         file_path);
    ignore (time (run "chan_try_in      " chan_try_in      ) file_path);
    ignore (time (run "chan_guard       " chan_guard       ) file_path);
    ignore (time (run "chan_guard_inline" chan_guard_inline) file_path);
  end


