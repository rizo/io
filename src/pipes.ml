
(*
 * Elements
 *)

module Fn = struct
  type ('a, 'b) t = 'a -> 'b
  let compose f g = fun x -> f (g x)
  let apply f x = f x
  let map f x = compose f x
  let id x = x
  let (@@) = apply
  let (@.) = compose
end

module T2 = struct
  type ('a, 'b) t = ('a * 'b)
  let map f (x, y) = (x, f y)
end

module Opt = struct
  type 'a t = 'a option

  exception No_value

  let value_exn opt =
    match opt with
    | Some x -> x
    | None -> raise No_value

  let value ~default opt =
    match opt with
    | Some x -> x
    | None -> default

  (*
   * Monad Implementation
   *)

  let return x =
    Some x

  let bind opt f =
    match opt with
    | Some x -> f x
    | None -> None

  let zero =
    return ()

  let delay f =
    f

  let combine opt dopt =
    bind opt (fun () -> dopt ())

  let run dopt = dopt ()

  let (>>=) = bind
  let (>>)  = combine

  (*
   * Monadic Combinators
   *)

  let rec forever opt =
    opt >> fun () -> forever opt

end

module Exn = struct
  let as_option e f x =
    try Some (f x)
    with e' when e' = e -> None

  let fail msg = raise (Failure msg)
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

module Base = struct
  let time f x =
    let t = Unix.gettimeofday () in
    let fx = f x in
    Printf.printf "Elapsed time: %f sec\n"
      (Unix.gettimeofday () -. t);
    fx

  let fail = Exn.fail
end

open Base

module Pipe = struct

  type ('i, 'o, 'v) t =
    | Value of 'v
    | Yield of ('o *  ('i, 'o, 'v) t)
    | Await of ('i -> ('i, 'o, 'v) t)

  (*
   * Functor Implementation
   *)

  let rec map f p =
    match p with
    | Value r -> Value  (f r)
    | Await k -> Await (Fn.map (map f) k)
    | Yield t -> Yield (T2.map (map f) t)

  (*
   * Monad Implementation
   *)

  let return x =
    Value x

  let rec bind m f =
    match m with
    | Value r       -> f r
    | Await k       -> Await (fun x -> bind (k x) f)
    | Yield (o, m') -> Yield (o, bind m' f)

  let combine m dm =
    bind m (fun () -> dm ())

  let (>>=) = bind
  let (>>)  = combine

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

  (* Receive input data. *)
  let await = Await (fun i -> Value i)

  (* Send output data. *)
  let yield o = Yield (o, Value ())

  (* Generic produces with a zero value. *)
  let zero : 'o producer = return ()

  (* Run can only _run_ pipelines that are complete.
     A complete pipeline is the one that awaits no value and yields no values. *)
  let rec run p =
    match p with
    | Value r          -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  let rec run_delayed dp =
    match dp () with
    | Value r          -> r
    | Await k          -> run_delayed (fun () -> (k Void))
    (* TODO: Should the p' already be delayed? *)
    | Yield (Void, p') -> run_delayed (fun () -> p')

  (* Pipe composition, fuse two pipes into one. *)
  let rec fuse p1 p2 =
    match (p1, p2) with
    | (Yield (o1, p1), p2            ) -> yield o1 >> fun () -> fuse p1 p2
    | (Value v1      , _             ) -> Value v1
    | (Await f1      , Yield (o2, p2)) -> fuse (f1 o2) p2
    | (p1            , Await f2      ) -> await >>= fun i -> fuse p1 (f2 i)
    | (_             , Value v2      ) -> Value v2

  let (<|<) p1 p2 = fuse p1 p2
  let (>|>) p2 p1 = fuse p1 p2

  (*
   * Pipe Combinators
   *)

  let rec forever m =
    m >> fun () -> forever m

  (* Create a pipe from a function. *)
  let pipe_forever f =
    forever (await >>= fun i -> yield (f i))

  let rec pipe f =
    await >>= fun x ->
    yield (f x) >> fun () ->
    pipe f

  let id () = pipe Fn.id

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard () =
    await >> fun () -> discard ()

  let rec print_forever () =
    forever (await >>= fun x -> return (print_endline x))

  (*
   * Common Producers
   *)

  let rec of_list l =
    match l with
    | [] -> return ()
    | x::xs -> yield x >> fun () -> of_list xs

  let rec of_channel chan =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> yield line >> fun () -> of_channel chan
    | None -> close_in chan; zero

  let rec get_line () : string producer =
    match Exn.as_option End_of_file read_line () with
    | Some line -> yield line >> fun () -> get_line ()
    | None -> zero

  let rec of_channel_exn chan =
    let rec loop () =
      try
        let line = input_line chan in
        yield line >> fun () -> loop ()
      with End_of_file ->
        close_in chan;
        return (lazy ()) in
    loop ()

  let rec of_channel_cont chan acc f =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> of_channel_cont chan (f acc line) f
    | None -> close_in chan; acc

  (*
   * Common Consumers
   *)

  (* FIXME: Return as consumer, i.e., within a value. *)
  let rec to_list
    : 'o producer -> 'a list
    = function
      | Value ()     -> []
      | Await k      -> fail "impossible output"
      | Yield (o, p') -> o :: to_list p'

  let rec put_line ()
    : (string, unit) consumer
    = await >>= fun x -> return (print_endline x) >> fun () -> put_line ()

  let rec put_line_forever ()
    : (string, unit) consumer
    = forever (await >>= fun x -> return (print_endline x))

end

