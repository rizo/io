
open Elements
open Flow


let test_monad () = begin
  assert (((fun () -> Ready ()) >>= return) () = Ready ());
  assert (run (yield 1 =>= (await >>= yield) => nth 0) = 1);
end

let test_api () = begin
  assert (run (of_list (List.range 10 100) =>= take 10 =>= tail => len) = 9);
  assert (run (of_list (List.iota 1000000) =>= take 10 =>= filter even => sum) = 20);
  assert (run (of_list (List.range 50 100) =>= take 10 => fold (+) 0) = 545);
  assert (run (iota 1000000 =>= filter odd => fold (+) 0) = 250000000000);
  assert (run (yield 1 >> yield 2 => collect) = [1; 2]);
end

let () =
  test_monad ();
  test_api ()

