
open Elements

module F1 = struct
  type ('i, 'o, 'a) action =
    | Ready of 'a
    | Yield of ('o *  ('i, 'o, 'a) action)
    | Await of ('i -> ('i, 'o, 'a) action)

  type 'o source = (void, 'o, void) action
  type ('i, 'a) sink = ('i, void, 'a) action
  type ('i, 'o) processor = ('i, 'o, void) action
  type 'a flow = (void, void, 'a) action

  let zero = Ready Void

  let return x = Ready x

  let rec (>>=) m f =
    match m with
    | Ready x       -> f x
    | Yield (o, m') -> Yield (o, m' >>= f)
    | Await k       -> Await (fun i -> k i >>= f)

  let (>>) a1 a2 =
    a1 >>= fun () -> a2

  let yield x = Yield (x, Ready ())

  let await = Await (fun i -> Ready i)

  let rec run a =
    match a with
    | Ready r         -> r
    | Await k         -> run (k Void)
    | Yield (Void, b) -> run b

  let rec fuse' a1 a2 =
    match (a1, a2) with
    | a2             , Yield (o1, a1) -> yield o1 >> fuse' a1 a2
    | _              , Ready x1       -> Ready x1
    | Yield (o2, a2) , Await f1       -> fuse' (f1 o2) a2
    | Await f2       , _              -> await >>= fun i -> fuse' a1 (f2 i)
    | Ready x2       , _              -> Ready x2

  and (>->) a1 a2 = fuse' a1 a2
  and (<-<) a2 a1 = fuse' a1 a2

  let rec fuse src snk : 'a flow =
    match (src, snk) with
    | a1            , Yield (x2, a2) -> yield x2 >> fuse a1 a2
    | _             , Ready r2       -> Ready r2
    | Yield (x1, a1), Await k2       -> fuse a1 (k2 x1)
    | _ -> fail "not compatible action types"

  let rec forever a = a >> forever a

  let rec discard : ('i, void) sink =
    Await (fun i -> discard)

  let rec yes : string source =
    Yield ("yes", yes)

  let nth n : ('a, 'a) sink =
    let rec loop i =
      Await (fun x -> if i = n
              then Ready x
              else loop (i + 1)) in
    loop 0

  let rec take n =
    if n = 0 then zero
    else Await (fun i -> Yield (i, take (n - 1)))

  let rec of_chan ch : string source =
    match Exn.as_option End_of_file input_line ch with
    | Some line -> yield line >> of_chan ch
    | None -> zero
end


module F2 = struct
  type ('i, 'o, 'a) action' =
    | Ready of 'a
    | Yield of ('o *  ('i, 'o, 'a) action)
    | Await of ('i -> ('i, 'o, 'a) action)
   and ('i, 'o, 'a) action = ('i, 'o, 'a) action' Lazy.t

  let return x = lazy (Ready x)

  let yield x = lazy (Yield (x, lazy (Ready ())))
end

