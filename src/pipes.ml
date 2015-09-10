
open Elements

module Pipe = struct

  (* Pipe state. *)
  type ('i, 'o, 'a) t =
    | Value of 'a
    | Yield of ('o *  ('i, 'o, 'a) t)
    | Await of ('i -> ('i, 'o, 'a) t)

  (*
   * Functor Implementation
   *)

  let rec fmap f p =
    match p with
    | Value v       -> Value  (f v)
    | Await k       -> Await (fun i -> fmap f (k i))
    | Yield (o, p') -> Yield (o, fmap f p')

  (*
   * Monad Implementation
   *)

  let return
    : 'a -> ('i, 'o, 'a) t
    = fun x -> Value x

  let rec (>>=)
    : ('i, 'o, 'a) t -> ('v -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
    = fun m f -> match m with
      | Value r       -> f r
      | Await k       -> Await (fun x -> k x >>= f)
      | Yield (o, m') -> Yield (o, m' >>= f)

  let (>>)
    : ('i, 'o, 'a) t -> ('i, 'o, 'b) t Lazy.t -> ('i, 'o, 'b) t
    = fun ma mb -> ma >>= fun () -> Lazy.force mb

  (*
   * Internal Implementation
   *)

  (* Void type, denotes blocked pipe end. *)
  type void = Void

  (* Effectful producer -- like generators, produces values from a source. *)
  type 'o producer = (void, 'o, unit) t

  (* Effectful consumers -- like iteratees, consume values and return values. *)
  type ('i, 'v) consumer = ('i,   void, 'v) t

  (* A complete pipeline, ready to be 'run'. *)
  type 'v pipeline = (void, void, 'v) t

  (* Generic produces with a zero value. *)
  let zero : 'o producer = return ()

  (* Receive input data. *)
  let await = Await (fun i -> Value i)

  (* Send output data. *)
  let yield o = Yield (o, Value ())

  (* Run can only _run_ pipelines that are complete.
     A complete pipeline is the one that awaits no value and yields no values. *)
  let rec run p =
    match p with
    | Value r          -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  (* Pipe composition, fuse two pipes into one. *)
  let rec fuse p1 p2 =
    match (p1, p2) with
    | (Yield (o1, p1), p2            ) -> yield o1 >> lazy (fuse p1 p2)
    | (Value v1      , _             ) -> Value v1
    | (Await f1      , Yield (o2, p2)) -> fuse (f1 o2) p2
    | (p1            , Await f2      ) -> await >>= fun i -> fuse p1 (f2 i)
    | (_             , Value v2      ) -> Value v2

  let (<<<) p1 p2 = fuse p1 p2
  let (>>>) p2 p1 = fuse p1 p2

  (*
   * Pipe Combinators
   *)

  let rec forever
    : ('i, 'o, unit) t -> ('i, 'o, unit) t
    = fun m -> m >> lazy (forever m)

  (* Identity pipe, passes the values. *)
  let id () = forever (await >>= yield)

  (* Create a pipe from a function. *)
  let map f =
    forever (await >>= fun i -> yield (f i))

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard () =
    await >> lazy (discard ())

  (*
   * Common Producers
   *)

  let rec of_list l =
    match l with
    | [] -> return ()
    | x::xs -> yield x >> lazy (of_list xs)

  let rec of_channel chan =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> yield line >> lazy (of_channel chan)
    | None -> close_in chan; zero

  let rec get_line () =
    match Exn.as_option End_of_file read_line () with
    | Some line -> yield line >> lazy (get_line ())
    | None -> zero

  let rec of_channel_exn chan =
    let rec loop () =
      try
        let line = input_line chan in
        yield line >> lazy (loop ())
      with End_of_file ->
        close_in chan;
        return (lazy ()) in
    loop ()

  let rec of_channel_cont chan acc f =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> of_channel_cont chan (f acc line) f
    | None -> close_in chan; acc

  let yes () =
    forever (yield "yes")

  (*
   * Common Consumers
   *)

  (* FIXME: Return as consumer, i.e., within a value. *)
  let rec to_list
    : 'o producer -> ('o, 'a list) consumer
    = fun p ->
      let rec loop acc p =
        match p with
        | Value ()     -> return (List.rev acc)
        | Await k      -> fail "impossible output"
        | Yield (o, p') -> loop (o :: acc) p' in
      loop [] p

  let rec put_line =
    forever (await >>= fun x -> return (print_endline x))

end

module P = Pipe

