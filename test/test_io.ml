
open Elements
open IO
open IO.Seq

let test_monad () = begin
  print_endline "test_monad";
  assert ((Ready None) >>= return = Ready None);
  assert (nth 0 (yield 1 => (await >>= yield)) = Some 1);
end

let test_sinks () = begin
  print_endline "test_sinks";
  assert (len (iota 10) = 10);
  assert (sum (iota 10) = 45);
  assert (last (range 20 26) = Some 25);
end

let test_api () = begin
  print_endline "test_api";
  assert (len (list (List.range 10 100) => take 10 => tail) = 9);
  assert (sum (list (List.range 10 100) => take 10 => filter even) = 70);
  assert (fold ~init:0 ~f:(+) (list (List.range 50 100) => take 10) = 545);
  assert (head (range 10 100) = Some 10);
  assert (sum (iota 100000000000 => take 10 => filter even) = 20);
  assert (nth 10 (iota 100000000000 => filter even) = Some 20);
  assert (fold ~init:0 ~f:(+) (iota 1000000 => filter odd) = 250000000000);
  assert (collect (yield 1 >> lazy (yield 2)) = [1; 2]);
end

let () = begin
  test_monad ();
  test_sinks ();
  test_api ();
end



