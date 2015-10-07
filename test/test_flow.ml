
open Elements
open Flow

let (==) x y =
  assert (x = y)

let (!=) x y =
  assert (x <> y)

let test_composition () = begin
  assert (Close <-< Close = Close);
  assert (Close >-> Close = Close);

  assert (Yield (1, Close) <-< Close
          = Yield (1, Close));

  assert (Yield (1, Close) >-> Close
          = Close);
end

let test_monad () = begin
  assert (Close >>= (fun x -> Yield (x, Close))
                    = Close);

  assert (Yield (1, Close) >>= (fun x -> Yield (x, Close))
                               = Yield (1, Close));

  assert (Yield (1, Close) >>= (fun x -> Yield (x, Yield (x + 1, Close)))
                               = (Yield (1, Yield (2, Close))));

  assert (Close >> Close = Close);

  assert (Yield ((), Close) >> Close
                               = Close);

  assert (Close >> Yield (1, Close)
                   = Close);
end

let test_api () = begin
  assert (yield 1
          = Yield (1, Close));

  (* Fails because of the composition operator. *)
  (* yield 1 >> yield 2                *)
  (*   == Yield (1, Yield (2, Close)); *)
end

let () =
  test_composition ();
  test_monad ();
  test_api ()

