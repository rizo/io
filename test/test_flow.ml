
open Elements
open Flow


let test_monad () = begin
  print_endline "test_monad";
  assert (((fun () -> Ready ()) >>= return) () = Ready ());
  assert (yield 1 =>= (await >>= yield) => nth 0 = Ok 1);
end


let test_sinks () = begin
  print_endline "test_sinks";
  assert (iota 10 => len = 10);
  assert (iota 10 => sum = 45);
  assert (range 20 26 => last = Some 25);
end

let test_api () = begin
  print_endline "test_api";
  assert (of_list (List.range 10 100) =>= take 10 =>= tail => len = 9);
  assert (of_list (List.range 10 100) =>= take 10 =>= filter even => sum = 70);
  assert (of_list (List.range 50 100) =>= take 10 => fold (+) 0 = 545);
  assert (iota 100000000000 =>= take 10 =>= filter even => sum = 20);
  assert (iota 1000000 =>= filter odd => fold (+) 0 = 250000000000);
  assert (yield 1 >> yield 2 => collect = [1; 2]);
end

let () = begin
  test_monad ();
  test_sinks ();
  test_api ();
end

