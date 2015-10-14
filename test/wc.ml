
open Flow

let () =
  let line_count =
    get_line_from_chan stdin => count in
  print_endline (string_of_int line_count)

