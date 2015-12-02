
open Elements
open IO

let test_monad () = begin
  print_endline "io_core: test_monad";
  assert ((Ready None) >>= return = Ready None);
end


let () = begin
  test_monad ();
end



