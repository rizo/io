
open Elements

module Pipe = struct

  (* Current state of the computation. *)
  type ('i, 'o, 'a) step =
    | Ready of 'a
    | Yield of ('o *  ('i, 'o, 'a) pipe)
    | Await of ('i -> ('i, 'o, 'a) pipe)

  (* Pipe is defined as a lazy step value. *)
   and ('i, 'o, 'a) pipe = ('i, 'o, 'a) step Lazy.t

  (*
   * Monad Implementation
   *)

  let return x = lazy (Ready x)

  let rec (>>=) p f =
    match Lazy.force p with
    | Ready r       -> f r
    | Yield (o, p') -> lazy (Yield (o, p' >>= f))
    | Await k       -> lazy (Await (fun x -> k x >>= f))

  let (>>) p1 p2 =
    p1 >>= fun () -> p2

  (*
   * Internal Implementation
   *)

  (* Void type, denotes blocked pipe end. *)
  type void = Void

  (* Effectful source -- like generators, produces values from a source. *)
  type 'o source = (void, 'o, unit) pipe

  (* Effectful sinks -- like iteratees, consume values and return values. *)
  type ('i, 'v) sink = ('i,   void, 'v) pipe

  (* A complete pipeline, ready to be 'run'. *)
  type ('i, 'o) transformer = ('i, 'o, unit) pipe

  (* A complete pipeline, ready to be 'run'. *)
  type 'v pipeline = (void, void, 'v) pipe

  (* Generic produces with a zero value. *)
  let zero () = return ()

  (* Receive input data. *)
  let await = lazy (Await (fun i -> lazy (Ready i)))

  (* Send output data. *)
  let yield o = lazy (Yield (o, lazy (Ready ())))

  (* Run can only _run_ pipelines that are complete.
     A complete pipeline is the one that awaits no value and yields no values. *)
  let rec run p =
    match Lazy.force p with
    | Ready r          -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  (* Pipe composition, fuse two pipes into one. *)
  let rec fuse p1' p2' =
    match Lazy.(force p1', force p2') with
    | Yield (x1, p1), p2             -> yield x1 >> fuse p1 p2'
    | Ready r1      , _              -> lazy (Ready r1)
    | Await f1      , Yield (x2, p2) -> fuse (f1 x2) p2
    | _             , Await f2       -> await >>= fun x -> fuse p1' (f2 x)
    | _             , Ready r2       -> lazy (Ready r2)

  and (>->) p2 p1 = fuse p1 p2
  and (<-<) p1 p2 = fuse p1 p2

  (*
   * Pipe Combinators
   *)

  let rec forever p = p >> forever p
  let rec forever' p =
    lazy begin
      match Lazy.force p with
      | Ready x       -> print"val";Ready x
      | Yield (o, p') -> print"yld";Yield (o, forever' p')
      | Await k       -> print"awt";Await (fun x -> forever' (k x))
    end

  (* Identity pipe, passes the values. *)
  let id () = forever (await >>= yield)

  (* Create a pipe from a function. *)
  let map_forever f =
    forever (await >>= fun i -> yield (f i))

  let rec map_test f =
    await >>= fun i -> lazy (Yield (f i, map_test f))

  let rec map f : ('i, 'o) transformer =
    lazy (Await (fun i -> lazy (Yield (f i, map f))))

  let rec map_forever' f =
    forever' (await >>= fun i -> yield (f i))

  let rec map_match f (p : 'o source) : 'o source =
    lazy begin
      match Lazy.force p with
      | Yield (o, p') -> Yield (f o, map_match f p')
      | p -> p
    end

  let rec take n : ('i, 'o) transformer =
    if n = 0 then zero ()
    else await >>= fun i -> yield i >> take (n - 1)

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard =
    lazy (Await (fun _ -> discard))

  (*
   * Common sources
   *)

  let rec of_list l =
    match l with
    | [] -> return ()
    | x::xs -> yield x >> of_list xs

  let rec of_channel ch =
    match Exn.as_option End_of_file input_line ch with
    | Some line -> yield line >> of_channel ch
    | None -> zero ()

  let open_file filename =
    let rec loop ch =
      match Exn.as_option End_of_file input_line ch with
      | Some line -> yield line >> loop ch
      | None -> close_in ch; zero () in
    loop (open_in filename)

  let rec get_line () =
    match Exn.as_option End_of_file read_line () with
    | Some line -> yield line >> get_line ()
    | None -> zero ()

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

  let rec yes : string source =
    lazy (Yield ("y", yes))

  let count : int source =
    let rec loop n =
      lazy (Yield (n, loop (n + 1))) in
    loop 0

  let iota n : int source =
    let rec loop i =
      lazy (if i = n then Ready ()
            else Yield (i, loop (i + 1))) in
    loop 0

  (*
   * Common sinks
   *)

  let rec to_list (p : 'o source) : 'o list pipeline=
    let rec loop acc p =
      match Lazy.force p with
      | Ready ()      -> Ready (List.rev acc)
      | Await k       -> fail "impossible output"
      | Yield (o, p') -> loop (o :: acc) p' in
    lazy (loop [] p)

  let put_line () =
    forever' (await >>= fun x -> return (print_endline x))

  let rec put_line' =
    lazy (Await (fun i -> print i; put_line'))

  let rec put_line' =
    lazy (Await (fun i -> print i; put_line'))
end

open Pipe

(* yes >> yes >-> take 5 >-> map String.uppercase |> to_list |> run; *)

