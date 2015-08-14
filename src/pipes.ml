
module Fn = struct
  type ('a, 'b) t = 'a -> 'b
  let comp f g = fun x -> f (g x)
  let map f x = comp f x
  let id x = x
end

module T2 = struct
  type ('a, 'b) t = ('a * 'b)
  let map f (x, y) = (x, f y)
end

module Coroutine = struct
  type ('i, 'o) coroutine = { run : ('i -> ('o * ('i, 'o) coroutine)) }

  let rec map : ('a -> 'b) -> ('i, 'a) coroutine -> ('i, 'b) coroutine =
    fun f c -> {
        run = fun i ->
          let (o, c') = c.run i in
          (f o, map f c')
      }
end



module Pipe = struct
  type ('a, 'b, 'r) t =
    | Pure  of 'r
    | Yield of ('b *  ('a, 'b, 'r) t)
    | Await of ('a -> ('a, 'b, 'r) t)

  let rec map f p =
    match p with
    | Pure  r -> Pure  (f r)
    | Await k -> Await (Fn.map (map f) k)
    | Yield t -> Yield (T2.map (map f) t)

  let pure x = Pure x

  let rec (<*>) f x =
    match f with
    | Pure r  -> map r x
    | Await k -> Await (Fn.map (fun f' -> f' <*> x) k)
    | Yield t -> Yield (T2.map (fun f' -> f' <*> x) t)

  let rec bind m f =
    match m with
    | Pure r  -> f r
    | Await k -> Await (Fn.map (fun m' -> bind m' f) k)
    | Yield t -> Yield (T2.map (fun m' -> bind m' f) t)

  let (>>=) m f = bind m f

  let seq m k = m >>= fun _ -> k

  let (>>) = seq

  let rec forever x () = x >> forever x ()

  type void = Void
  type ('b, 'r) producer = (void, 'b,   'r) t
  type ('a, 'r) consumer = ('a,   void, 'r) t
  type     ('r) pipeline = (void, void, 'r) t

  let await   = Await (fun r -> Pure r)
  let yield x = Yield (x, Pure ())

  (* val pipe : (a -> b) -> Pipe a b m r *)
  (* let pipe_forever f = forever (await >>= Fn.comp yield f) *)
  let rec pipe f = await >>= fun x -> yield (f x) >> pipe f

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard () = await >> discard ()

  let id () = pipe Fn.id

  let rec comp p1 p2 =
    match (p1, p2) with
    | (Yield (x1, p1), p2            ) -> yield x1 >> comp p1 p2
    | (Pure r1       , _             ) -> Pure r1
    | (Await f1      , Yield (x2, p2)) -> comp (f1 x2) p2
    | (p1            , Await f2      ) -> await >>= fun x -> comp p1 (f2 x)
    | (_             , Pure r2       ) -> Pure r2

  let (<<<) p1 p2 = comp p1 p2
  let (>>>) p2 p1 = comp p1 p2

  let rec run p =
    match p with
    | Pure r           -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  (* val print : unit -> ('a, void, unit) t *)
  let rec print () =
    await >>= fun x -> pure (print_endline x) >> print ()

  let map f = pipe f

  let rec of_channel ch =
    let rec go () =
      try
        let line = input_line ch in
        yield line >> go ()
      with End_of_file ->
        close_in ch;
        pure () in
    go ()

end


open Pipe





