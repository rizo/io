
open Elements
open Flow


let test_api () = begin
  assert (source_list (List.range 10 100) =>= take 10 =>= tail => len = 9);
  assert (source_list (List.iota 1000) =>= take 10 =>= filter even => sum = 20);
  assert (source_list (List.range 50 100) =>= take 10 => fold (+) 0 = 545);
  assert (iota 1000000 =>= filter odd => fold (+) 0 = 250000000000);
end

let () =
  test_api ()

