
open Elements
open Flow

let () =
  infinity
  =>= drop 5
  =>= take 10
  =>= filter even
  =>= map string_of_int
  =>  print

let () =
  let hosts = get_line_from_chan (open_in "/etc/hosts") in
  hosts >> hosts
           =>= map String.length
           =>= map string_of_int
           =>  print

