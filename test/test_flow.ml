
open Elements
open Flow

let test_composition () = begin
  assert (Value () <-< Value () = Value ());
  assert (Value () >-> Value () = Value ());

  assert (Yield (1, Value ()) <-< Value ()
          = Yield (1, Value ()));

  assert (Yield (1, Value ()) >-> Value ()
          = Value ());
end

let test_monad () = begin
  assert (Value () >>= (fun () -> Value ())
                       = Value ());

  assert (Value 1 >>= (fun x -> Yield (x, Value ()))
                      = Yield (1, Value ()));

  assert (Value 1 >>= (fun x -> Yield (x, Yield (x + 1, Value ())))
                      = Yield (1, Yield (2, Value ())));

  assert (Value () >> Value ()
                      = Value ());
end

let test_api () = begin
  assert (yield 1
          = Yield (1, Value ()));

  assert (yield 1 >> yield 2
                     = Yield (1, Yield (2, Value ())));
end

let () =
  test_composition ();
  test_monad ();
  test_api ()

