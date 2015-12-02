
open Elements
open IO.Proc

let experiments () = begin
  let p = proc "rev" in
  output_line p.stdin "hello";
  print (input_line p.stdout)
end

let () = begin
  experiments ();
end

