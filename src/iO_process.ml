
open Elements
open IO

type proc = {
  stdin  : out_channel;
  stdout : in_channel;
  stderr : in_channel;
}

let close p =
  Unix.close_process_full (p.stdout, p.stdin, p.stderr)


let proc ?(env = [||]) cmd =
  let (stdout, stdin, stderr) =
    Unix.open_process_full cmd env in
  try
    let errmsg = input_line stderr in
    raise (Failure errmsg)
  with End_of_file -> { stdin; stdout; stderr }


let run ?(env = [||]) cmd =
  let (stdout, stdin, stderr) =
    Unix.open_process_full cmd env in
  try
    let errmsg = input_line stderr in
    Result (Error errmsg)
  with End_of_file -> begin
      let p = { stdin; stdout; stderr } in
      let rec loop =
        Server (function
            | Some input ->
              output_line p.stdin input;
              (input_line p.stdout, loop)
            | None -> ("", Result (Ok (close p)))) in
      loop
    end





(* file :: Path -> Source String      *)
(* file path =                        *)
(*   guard (loop chan) >> close chan  *)
(*     where chan = open path         *)
(*      loop chan =                   *)
(*        try yield (input_line chan) *)
(*        with End_of_file -> empty   *)
(*     end                            *)

