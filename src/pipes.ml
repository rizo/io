

module Fn = struct
  type ('a, 'b) t = 'a -> 'b
  let comp f g = fun x -> f (g x)
  let map f x = comp f x
  let id x = x
end

module Linked_list = struct
  type 'a t = Nil | Cons of 'a * 'a t

  let rec rev xs =
    let rec loop acc xs =
      match xs with
      | Nil -> acc
      | Cons (x, xs') -> loop (Cons (x, acc)) xs'
    in
      loop Nil xs

  let rec map xs f =
    match xs with
    | Nil -> Nil
    | Cons (x, xs') -> Cons (f x, map xs' f)

  let map_opt xs f =
    let rec loop acc xs f =
      match xs with
      | Nil -> rev acc
      | Cons (x, xs') -> loop (Cons (f x, acc)) xs' f
    in
      loop Nil xs f

  let rec len xs =
    match xs with
    | Nil -> 0
    | Cons (_, xs') -> 1 + len xs'

  let rec len_opt xs =
    let rec loop n xs =
      match xs with
      | Nil -> 0
      | Cons (_, xs') -> loop (n + 1) xs'
    in
      loop 0 xs
end

let time f x =
  let t = Unix.gettimeofday () in
  let fx = f x in
  Printf.printf "Elapsed time: %f sec\n"
    (Unix.gettimeofday () -. t);
  fx

module T2 = struct
  type ('a, 'b) t = ('a * 'b)
  let map f (x, y) = (x, f y)
end

module Opt = struct
  exception No_value

  let value opt =
    match opt with
    | Some x -> x
    | None -> raise No_value
end

module Exn = struct

  let as_option e f x =
    try Some (f x)
    with e' when e' = e -> None

  let fail msg = raise (Failure msg)
end

let fail = Exn.fail

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
    | Value of 'r
    | Yield of ('b *  ('a, 'b, 'r) t)
    | Await of ('a -> ('a, 'b, 'r) t)

  let rec map f p =
    match p with
    | Value r -> Value  (f r)
    | Await k -> Await (Fn.map (map f) k)
    | Yield t -> Yield (T2.map (map f) t)

  let return x = Value x

  let rec (<*>) f x =
    match f with
    | Value r -> map r x
    | Await k -> Await (Fn.map (fun f' -> f' <*> x) k)
    | Yield t -> Yield (T2.map (fun f' -> f' <*> x) t)

  let rec bind m f =
    match m with
    | Value r       -> f r
    | Await k       -> Await (fun x -> bind (k x) f)
    | Yield (o, m') -> Yield (o, bind m' f)

  let (>>=) m f = bind m f

  let seq m k = m >>= fun _ -> k

  let (>>) = seq

  let rec forever x () = x >> forever x ()

  type void = Void

  (* Effectful producer -- like generators, produces values from a source. *)
  type ('b, 'r) producer = (void, 'b,   'r) t

  (* Effectful consumers -- like iteratees, consume values and return values. *)
  type ('a, 'r) consumer = ('a,   void, 'r) t

  type     ('r) pipeline = (void, void, 'r) t

 (* Receive input data. *)
  let await   = Await (fun r -> Value r)

  (* Send output data. *)
  let yield x = Yield (x, Value ())

  (* val pipe : (a -> b) -> Pipe a b m r *)
  (* let pipe_forever f = forever (await >>= Fn.comp yield f) *)
  let rec pipe f = await >>= fun x -> yield (f x) >> pipe f

  (* The 'discard' pipe silently discards all input fed to it. *)
  let rec discard () = await >> discard ()

  let id () = pipe Fn.id

  let rec fuse p1 p2 =
    match (p1, p2) with
    | (Yield (x1, p1), p2            ) -> yield x1 >> fuse p1 p2
    | (Value r1      , _             ) -> Value r1
    | (Await f1      , Yield (x2, p2)) -> fuse (f1 x2) p2
    | (p1            , Await f2      ) -> await >>= fun x -> fuse p1 (f2 x)
    | (_             , Value r2      ) -> Value r2

  module Syntax = struct
    let (<<<) p1 p2 = fuse p1 p2
    let (>>>) p2 p1 = fuse p1 p2
    let (>>=) m f = bind m f
    let (>>) = seq
  end

  (* Run can only _run_ pipelines that are complete.
     A complete pipeline is the one that awaits no value and yields no values. *)
  let rec run p =
    match p with
    | Value r          -> r
    | Await k          -> run (k Void)
    | Yield (Void, p') -> run p'

  (* val print : unit -> ('a, void, unit) t *)
  let rec print () =
    await >>= fun x -> return (print_endline x) >> print ()

  let rec map f =
    await >>= fun x ->
    yield (f x) >>
    map f

  let rec from_channel_exn chan =
    let rec loop () =
      try
        let line = input_line chan in
        yield line >> loop ()
      with End_of_file ->
        close_in chan;
        return () in
    loop ()



  let rec from_channel' chan =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> yield line >>= fun _ -> from_channel' chan
    | None -> close_in chan; return ()

  let rec from_channel_fold chan acc f =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> from_channel_fold chan (f acc line) f
    | None -> close_in chan; acc

   (* (f acc line) f *)

   (* k   :: (a -> b -> a) *)
   (* acc :: a *)
   (* x   :: b *)

  let rec from_channel_fold' chan =
    match Exn.as_option End_of_file input_line chan with
    | Some line -> yield line >> from_channel_fold' chan
    | None -> close_in chan; return ()

  let rec cat file_path acc f =
    let chan = open_in file_path in
    from_channel_fold chan acc f

  let rec len xs =
    match xs with
    | `Nil -> 0
    | `Cons (_, xs') -> 1 + len xs'

  let len await =
    await 0 (fun total x -> total + 1)

  let len =
    let rec step n next =
      match next with
      | None   -> Yield (n, Value ())
      | Some x -> Await (step (n + 1))
    in
      Await (fun i -> step 0 i)

end

open Pipe
open Pipe.Syntax

module Input = struct
  type 'a t = 'a option
end


(*
 * Consumers
 *)

(* -- | Convert a pure 'Producer' into a list   *)
(* toList :: Producer a Identity () -> [a]      *)
(* toList = loop                                *)
(*   where                                      *)
(*     loop p = case p of                       *)
(*         Request v _  -> closed v             *)
(*         Respond a fu -> a:loop (fu ())       *)
(*         M         m  -> loop (runIdentity m) *)
(*         Pure    _    -> []                   *)

let rec to_list
  : ('a, unit) producer -> 'a list
  = fun p ->
    match p with
    | Value ()     -> []
    | Await k      -> fail "impossible output"
    | Yield (o, p') -> o :: to_list p'

(* let to_list : ('a Input.t, 'a list) consumer = *)
(*   let rec go acc input = *)
(*     match input with *)
(*     | Some x -> Await (fun input -> go (x::acc) input) *)
(*     | None   -> Value (List.rev acc) *)
(*   in *)
(*     Await (fun input -> go [] input) *)

(*
 * Producers
 *)

let rec of_list l : ('a, unit) producer =
  match l with
  | [] -> return ()
  | x::xs -> yield x >> of_list xs

let rec of_channel chan =
  match Exn.as_option End_of_file input_line chan with
  | Some line -> yield line >> of_channel chan
  | None -> close_in chan; return ()

(* stdinLn :: Producer String IO () *)
(*  stdinLn = do *)
(*      eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad *)
(*      unless eof $ do *)
(*          str <- lift getLine *)
(*          yield str            -- 'yield' the 'String' *)
(*          stdinLn              -- Loop *)

let rec get_line () : (string, unit) producer =
  match Exn.as_option End_of_file read_line () with
  | Some line -> yield line >> get_line ()
  | None -> return ()

(* stdoutLn :: Consumer String IO () *)
(* stdoutLn = do *)
(*    str <- await  -- 'await' a 'String' *)
(*    x   <- lift $ try $ putStrLn str *)
(*    case x of *)
(*        -- Gracefully terminate if we got a broken pipe error *)
(*        Left e@(G.IOError { G.ioe_type = t}) -> *)
(*            lift $ unless (t == G.ResourceVanished) $ throwIO e *)
(*        -- Otherwise loop *)
(*        Right () -> stdoutLn *)

let rec put_line
  : unit -> (string, unit) consumer
  = fun () ->
    await >>= fun x -> return (print_endline x) >> put_line ()


let rec to_list
  : ('a, unit) producer -> 'a list
  = fun p ->
    match p with
    | Value ()     -> []
    | Await k      -> fail "impossible output"
    | Yield (o, p') -> o :: to_list p'

let rec enum_list l p =
  match (p, l) with
  | Await k, x::xs -> enum_list xs (k x)
  | _-> p

let rec get_line p : (string, unit) producer =
  match p with
  | Await k ->
    begin match Exn.as_option End_of_file read_line () with
      | Some line -> get_line (k line)
      | None -> return ()
    end
  | _ -> return ()

let enum_file file_path i =
  let chan = open_in file_path in
  let rec enum i =
    match i with
    | Await k ->
      begin match Exn.as_option End_of_file input_line chan with
        | Some line -> enum (k line)
        | None -> close_in chan; return ()
      end
    | _ -> close_in chan; return () in
  enum i

let apply source pipe : ('a, unit) producer =
  match pipe with
  | Await k -> source k
  | _ -> return ()

let ($) = apply

let rec get_line k : (string, unit) producer =
  begin match Exn.as_option End_of_file read_line () with
    | Some line -> get_line $ (k line)
    | None -> return ()
  end



