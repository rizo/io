
open Elements
open IO
open IO.Iter

let test_sinks () = begin
  print_endline "io_iter: test_sinks";
  assert (length (iota 10) = 10);
  assert (sum (iota 10) = 45);
  assert (last (range 20 26) = Some 25);
end

let test_slice () = begin
  print "io_iter: test_slice";

  assert (collect (count => slice 0 0) = []);
  assert (collect (count => slice 0 1) = [0]);
  assert (collect (count => slice 3 8) = [3; 4; 5; 6; 7]);
end

let test_api () = begin
  print "io_iter: test_api";

  assert (fold ~init:0 ~f:(+) (iota 1000000 => filter odd) = 250000000000);
  assert (collect (count => take 5) = [0; 1; 2; 3; 4]);
  assert (length (list (List.range 10 100) => take 10 => tail) = 9);
  assert (sum (list (List.range 10 100) => take 10 => filter even) = 70);
  assert (fold ~init:0 ~f:(+) (list (List.range 50 100) => take 10) = 545);
  assert (head (range 10 100) = Some 10);
  assert (sum (iota 100000000000 => take 10 => filter even) = 20);
  assert (nth 10 (iota 100000000000 => filter even) = Some 20);
  assert (fold ~init:0 ~f:(+) (iota 1000000 => filter odd) = 250000000000);
  assert (collect (yield 1 >> lazy (yield 2)) = [1; 2]);
end

let () = begin
  test_sinks ();
  test_slice ();
  test_api ();
end

