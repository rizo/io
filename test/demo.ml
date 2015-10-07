
open Pipes

let _ =
  let hosts = Pipe.of_channel (open_in "/etc/hosts") in
  hosts >> hosts >>>
    map String.length >>>
    map string_of_int >>>
    print ()

