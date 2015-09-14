
open Elements

module Pipe = struct

  (* Current state of the computation. *)
  type ('i, 'o, 'a) step =
    | Value of 'a
    | Yield of ('o *  ('i, 'o, 'a) pipe)
    | Await of ('i -> ('i, 'o, 'a) pipe)

  (* Pipe is defined as a lazy step value. *)
   and ('i, 'o, 'a) pipe = ('i, 'o, 'a) step Lazy.t

  (*
   * Monad Implementation
   *)

  let return x = lazy (Value x)

  let rec (>>=) p f =
    match Lazy.force p with
    | Value r       -> f r
    | Yield (o, p') -> lazy (Yield (o, p' >>= f))
    | Await k       -> lazy (Await (fun x -> k x >>= f))

  let (>>) p1 p2 =
    p1 >>= fun () -> p2

  (*
   * Internal Implementation
   *)

  (* Void type, denotes blocked pipe end. *)
  type void = Void

  (* Effectful producer -- like generators, produces values from a source. *)
  type 'o producer = (void, 'o, unit) pipe

  (* Effectful consumers -- like iteratees, consume values and return values. *)
  type ('i, 'v) consumer = ('i,   void, 'v) pipe

  (* A complete pipeline, ready to be 'run'. *)
  type 'v pipeline = (void, void, 'v) pipe

  (* Generic produces with a zero value. *)
  let zero : 'o producer = return ()

  (* Receive input data. *)
  let await = lazy (Await (fun i -> lazy (Value i)))

  (* Send output data. *)
  let yield o = lazy (Yield (o, lazy (Value ())))

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
    | (Yield (o, p), _)            -> yield o >> fuse p p2
    | (Value _     , _)            -> p1
    | (Await f     , Yield (o, p)) -> fuse (f o) p
    | (_           , Await f)      -> await >>= fun i -> fuse p1 (f i)
    | (_           , Value _)      -> p2

  let (<<<) p1 p2 = fuse p1 p2
  let (>>>) p2 p1 = fuse p1 p2

  (*
   * Pipe Combinators
   *)

  let rec forever p = p >> forever p

  (* Identity pipe, passes the values. *)
  let id () = forever (await >>= yield)

  (* Create a pipe from a function. *)
  let map_forever f =
    forever (await >>= fun i -> yield (f i))

  let rec map f (p : 'o producer) : 'o producer =
    lazy begin
      match Lazy.force p with
      | Yield (o, p') -> Yield (f o, map f p')
      | p -> p
    end

  let rec take n p =
    if n = 0 then zero
    else lazy begin
      match Lazy.force p with
      | Yield (o, p') -> Yield (o, (take (n - 1) p'))
      | p -> p
    end

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

  let yes_forever () =
    forever (yield "yes")

  let rec yes =
    lazy (Yield ("y", yes))

  let count () : int producer =
    let rec loop n =
      lazy (Yield (n, loop (n + 1))) in
    loop 0

  (*
   * Common Consumers
   *)

  let rec to_list (p : 'o producer) : 'o list pipeline=
    let rec loop acc p =
      match Lazy.force p with
      | Value ()      -> Value (List.rev acc)
      | Await k       -> fail "impossible output"
      | Yield (o, p') -> loop (o :: acc) p' in
    lazy (loop [] p)

  let put_line () =
    forever (await >>= fun x -> return (print_endline x))

end

module P = Pipe

