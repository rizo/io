
open Elements
open IO_core

type proc = {
  stdin  : out_channel;
  stdout : in_channel;
  stderr : in_channel;
}

exception Process_cretion_failure of string

let proc ?(env = [||]) cmd =
  Log.info (fmt "Will create a process with cmd: `%s`..." cmd);
  let (stdout, stdin, stderr) =
    Unix.open_process_full cmd env in
    Log.info "Created.";
  try
    Log.info "Checking for errors...";
    Unix.set_nonblock (Unix.descr_of_in_channel stderr);
    Unix.sleep 1;
    let err_msg = input_line stderr in
    Log.info "An error occurred...";
    Unix.clear_nonblock (Unix.descr_of_in_channel stderr);
    raise (Process_cretion_failure err_msg)
  with End_of_file | Sys_blocked_io ->
    Log.info "Ok, no errors.";
    { stdin; stdout; stderr }

let close p =
  Unix.close_process_full (p.stdout, p.stdin, p.stderr)

(* IO Producer *)

let rec iter proc =
  let rec loop () =
    yield (input_line proc.stdout) >> lazy (loop ()) in
  try loop ()
  with End_of_file -> return ()

let stop = close

