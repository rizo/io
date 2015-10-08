
open Elements
open Flow

let test_composition () = begin
  assert (Close =<= Close = Close);
  assert (Close =>= Close = Close);

  assert (Yield (1, Close) =<= Close
          = Yield (1, Close));

  assert (Yield (1, Close) =>= Close
          = Close);
end


let test_api () = begin
  assert (yield 1
          = Yield (1, Close));

  assert (yield 1 >> yield 2
                     = Yield (1, Yield (2, Close)));

  assert ((of_list [1; 2; 3; 4; 5] =>= take 4 =>= nth_platonic 2)
          = Value 3);

  assert ((of_list [1; 2; 3; 4; 5] =>= take 4 =>= nth_platonic 2) = Value 3);
  assert ((of_list [1; 2; 3; 4; 5] =>= take 4 =>= nth_platonic 0) = Value 1);
  assert ((of_list [1; 2; 3; 4; 5] =>= take 4 =>= nth_platonic 4) = Close);

  (* Temporary maps values to options to simulate stream ending. *)
  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) =>= nth_opt_input 3)
     = Value 4);

  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) =>= nth_opt_input 100)
          = Error (Failure "nth: index out of range"));

  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) =>= nth_opt_input (-1))
          = Error (Invalid_argument "nth: negative index"));

  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) |> nth_direct 3)
          = Value (Some 4));

  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) |> nth_direct 100)
          = Error (Failure "nth: index out of range"));

  assert ((of_list (List.map (function 0 -> None | n -> Some n) [1;2;3;4;5;0]) |> nth_direct (-1))
          = Error (Invalid_argument "nth: negative index"));
end

let () =
  test_composition ();
  test_api ()

