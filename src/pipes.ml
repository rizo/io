
open Elements

module Pipe = struct

  (* Pipe state. *)
  type ('i, 'o, 'a) pipe =
    | Value of 'a
    | Yield of ('o *  ('i, 'o, 'a) lazy_pipe)
    | Await of ('i -> ('i, 'o, 'a) lazy_pipe)
  and
    ('i, 'o, 'a) lazy_pipe = ('i, 'o, 'a) pipe lazy_t

  (*
   * Functor Implementation
   *)

  let rec fmap f p =
    lazy begin match Lazy.force p with
      | Value v       -> Value  (f v)
      | Await k       -> Await (fun i -> fmap f (k i))
      | Yield (o, p') -> Yield (o, fmap f p')
    end

  (*
   * Monad Implementation
   *)

  let return
    : 'a -> ('i, 'o, 'a) lazy_pipe
    = fun x -> lazy (Value x)

  let rec (>>=)
     : ('i, 'o, 'a) lazy_pipe
    -> ('v -> ('i, 'o, 'b) lazy_pipe)
    -> ('i, 'o, 'b) lazy_pipe
    = fun m f -> match Lazy.force m with
      | Value r       -> f r
      | Await k       -> lazy (Await (fun x -> k x >>= f))
      | Yield (o, m') -> lazy (Yield (o, m' >>= f))

  let (>>)
     : ('i, 'o, 'a) lazy_pipe
    -> ('i, 'o, 'b) lazy_pipe
    -> ('i, 'o, 'b) lazy_pipe
    = fun ma mb -> ma >>= fun _ -> mb

  (*
   * Internal Implementation
   *)

  (* Void type, denotes blocked pipe end. *)
  type void = Void

  (* Effectful producer -- like generators, produces values from a source. *)
  type 'o producer = (void, 'o, unit) lazy_pipe

  (* Effectful consumers -- like iteratees, consume values and return values. *)
  type ('i, 'v) consumer = ('i,   void, 'v) lazy_pipe

  (* A complete pipeline, ready to be 'run'. *)
  type 'v pipeline = (void, void, 'v) lazy_pipe

  (* Generic produces with a zero value. *)
  let zero : 'o producer = return ()

  (* Receive input data. *)
  let await : ('i, 'o, 'a) lazy_pipe =
    lazy (Await (fun i -> lazy (Value i)))

  (* Send output data. *)
  let yield o : ('i, 'o, 'a) lazy_pipe =
    lazy (Yield (o, zero))

  (* Run can only _run_ pipelines that are complete.
     A complete pipeline is the one that awaits no value and yields no values. *)
  let rec run p =
    match Lazy.force p with
    | Value r          -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  (* Pipe composition, fuse two pipes into one. *)
  let rec fuse p1 p2 =
    match Lazy.(force p1, force p2) with
      | (Yield (o, p), _           ) -> yield o >> fuse p1 p2
      | (Value x     , _           ) -> lazy (Value x)
      | (Await f     , Yield (o, p)) -> fuse (f o) p
      | (_           , Await f     ) -> await >>= fun i -> fuse p1 (f i)
      | (_           , Value x     ) -> lazy (Value x)

  let (<<<) p1 p2 = fuse p1 p2
  let (>>>) p2 p1 = fuse p1 p2

  (*
   * Pipe Combinators
   *)

  let rec forever
    : ('i, 'o, unit) lazy_pipe -> ('i, 'o, unit) lazy_pipe
    = fun m -> m >> forever m

  (* Identity pipe, passes the values. *)
  let id () = forever (await >>= yield)

  (* Create a pipe from a function. *)
  let map f =
    forever (await >>= fun i -> yield (f i))

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard () =
    await >> discard ()

  (*
   * Common Producers
   *)

  let rec of_list l =
    match l with
    | [] -> return ()
    | x::xs -> yield x >> of_list xs

  let rec of_channel ch =
    match Exn.as_option End_of_file input_line ch with
    | Some line -> yield line >> of_channel ch
    | None -> zero

  let open_file filename =
    let rec loop ch =
      match Exn.as_option End_of_file input_line ch with
      | Some line -> yield line >> loop ch
      | None -> close_in ch; zero in
    loop (open_in filename)

  let rec get_line () =
    match Exn.as_option End_of_file read_line () with
    | Some line -> yield line >> get_line ()
    | None -> zero

  let rec of_channel_exn chan =
    let rec loop () =
      try
        let line = input_line chan in
        yield line >> loop ()
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
        match Lazy.force p with
        | Value ()      -> return (List.rev acc)
        | Await k       -> fail "impossible output"
        | Yield (o, p') -> loop (o :: acc) p' in
      loop [] p

  let rec put_line () =
    forever (await >>= fun x -> return (print_endline x))

end

module P = Pipe

