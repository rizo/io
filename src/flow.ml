

module Bi_pipes = struct
  type ('a_req, 'a_res, 'b_req, 'b_res, 'ret) node =
    | Req of 'a_req * ('a_res -> ('a_req, 'a_res, 'b_req, 'b_res, 'ret) node)
    | Res of 'b_req * ('b_res -> ('a_req, 'a_res, 'b_req, 'b_res, 'ret) node)
    | Ret of 'ret

  let return x = Ret x

  let rec (>>=) n0 f =
    let rec go n =
      match n with
      | Req (ao, k_ai) -> Req (ao, fun ai -> go (k_ai ai))
      | Res (bo, k_bi) -> Res (bo, fun bi -> go (k_bi bi))
      | Ret r          -> f r in
    go n0

  let (>>) s1 s2 =
    s1 >>= fun _ -> s2

  let respond o = Res (o, fun b -> Ret b)
  let request a = Req (a, fun i -> Ret i)
end

module Bi = struct
  (* type ('a_out, 'a_in, 'b_out, 'b_in, 'r) node = *)
  (* | Req of ('a_out  * ('a_in -> ('a_out, 'a_in, 'b_out, 'b_in, 'r) node)) *)
  (* | Res of ('b_in  -> ('b_out * ('a_out, 'a_in, 'b_out, 'b_in, 'r) node)) *)
  (* | Ret of 'r *)

  type ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node =
    | Client of ('a_req  * ('a_rep -> ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node))
    | Server of ('b_req -> ('b_rep  * ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node))
    | Result of 'r

  let return x = Result x

  let rec (>>=) n0 f =
    let rec go n =
      match n with
      | Client (req, h) -> Client (req, fun rep -> go (h req))
      | Server k        -> Server (fun req -> let rep, n' = k req in (rep, go n'))
      | Result r        -> f r in
    go n0

  let (>>) s1 s2 =
    s1 >>= fun _ -> s2

  let rep x = Server (fun req -> x, Result ())
  let req x = Client (x, fun rep -> Result rep)

  let rec (>>>) n1 n2 =
    match n1, n2 with
    | _,        Result r        -> Result r
    | _,        Server k        -> Server (fun req -> let rep, n' = k req in (rep, n1 >>> n'))
    | Server k, Client (req, h) -> let rep, n' = k req in n' >>> h rep
    | Client (req, h), Client _ -> Client (req, fun rep -> h rep >>> n2)
    | Result r       , Client _ -> Result r
  let (<<<) n2 n1 = n1 >>> n2
end


open Elements

type ('a, 'b, 'r) flow = ('a, 'b, 'r) node Lazy.t
and ('a, 'b, 'r) node =
  | Ready of 'r
  | Yield of ('b  * ('a, 'b, 'r) flow)
  | Await of ('a -> ('a, 'b, 'r) flow)
type ('a, 'b, 'r) t = ('a, 'b, 'r) flow

type    ('b, 'r) source = (void, 'b,   'r) flow
type ('a, 'b) processor = ('a,   'b, void) flow
type ('a, 'r)      sink = ('a, void,   'r) flow

let return x =
  lazy (Ready x)

let empty =
  lazy (Ready None)

let rec (>>=) s f =
  match force s with
  | Ready r       -> f r
  | Yield (o, s') -> lazy (Yield (o, s' >>= f))
  | Await k       -> lazy (Await (fun i -> k i >>= f))

let (>>) s1 s2 =
  s1 >>= fun _ -> s2
let (<<) s2 s1 = s1 >> s2

let yield b =
  lazy (Yield (b, return ()))

let await =
  lazy (Await (fun a -> return a))

let rec compose s1 s2 =
  match force s1, force s2 with
  | Ready r        , _              -> lazy (Ready r)
  | Yield (b, s1') , _              -> lazy (Yield (b, compose s1' s2))
  | Await k        , Yield (b, s2') -> compose (k b) s2'
  | Await _        , Await k        -> lazy (Await (fun a -> compose s1 (k a)))
  | Await _        , Ready r        -> lazy (Ready r)

let (<=) s1 s2 = compose s1 s2
let (=>) s2 s1 = compose s1 s2

let rec cat =
  lazy (Await (fun i -> lazy (Yield (i, cat))))

let rec run s =
  match force s with
  | Ready r       -> r
  | Await k       -> run (k Void)
  | Yield (a, s') -> run s'

let next source =
  match force source with
  | Ready _       -> None
  | Yield (a, s') -> Some (a, s')
  | Await k       -> fail "node is still awaiting input"

let rec get_line_from_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> lazy (Yield (line, get_line_from_chan ch))
  | None -> return ()

let rec get_char_from_chan ch =
  match Exn.as_option End_of_file input_char ch with
  | Some line -> lazy (Yield (line, get_char_from_chan ch))
  | None -> empty

let rec get_line_from_file file_path =
  let ch = open_in file_path in
  let rec get ch = match Exn.as_option End_of_file input_line ch with
    | Some line -> lazy (Yield (line, get ch))
    | None -> print_endline "closing chan"; close_in ch; empty in
  get ch


let rec of_list xs =
  match xs with
  | x::xs' -> lazy (Yield (x, of_list xs'))
  | []     -> empty

let rec map f =
  lazy (Await (fun a -> lazy (Yield (f a, map f))))

let rec filter pred =
  lazy (Await (fun a ->
      if pred a
      then lazy (Yield (a, filter pred))
      else filter pred))

let rec take n =
  if n < 0 then
    raise (Invalid_argument "take: negative value")
  else if n = 0
  then empty
  else lazy (Await (fun i -> lazy (Yield (i, take (n - 1)))))

let rec drop n =
  if n = 0
  then cat
  else lazy (Await (fun a -> drop (n - 1)))

let tail =
  lazy (Await (fun a -> cat))

let rec repeat x =
  lazy (Yield (x, repeat x))

let yes = repeat "y"

let rec count =
  let rec go n =
    lazy (Yield (n, go (n + 1))) in
  go 0

let rec range start stop =
  count => take stop => drop start

let rec range n m =
  count => take m => drop n

let rec iota stop =
  count => take stop

(* Sinks *)

let fold f z source =
  let rec go acc source =
    match next source with
    | Some (a, rest) -> go (f acc a) rest
    | None           -> acc in
  go z source

let rec put_line_direct source =
  match next source with
  | Some (a, rest) -> print_endline a; put_line_direct rest
  | None -> ()

let rec put_line =
  lazy (Await (fun line -> print line; put_line))

let rec inspect =
  lazy (Await (fun i -> print_endline i; lazy (Yield (i, inspect))))

let nth n =
  if n < 0 then
    raise (Invalid_argument "nth: negative index")
  else
    let rec loop n =
      lazy (Await (fun i -> if n = 0
                    then return (Some i) else loop (n - 1)))
    in loop n

let nth_direct n source =
  if n < 0 then
    Error (Invalid_argument "nth: negative index")
  else
    let rec loop n source =
      match next source with
      | None           -> Error (Failure "nth: empty node")
      | Some (a, rest) -> if n = 0 then Ok a else loop (n - 1) rest
    in loop n source

let collect src =
  let rec go acc src =
    match next src with
    | Some (a, rest) -> go (a::acc) rest
    | None -> List.rev acc
  in go [] src

let rec discard =
  lazy (Await (fun _ -> discard))

let head p =
  match next p with
  | Some (a, _) -> Ok a
  | None -> Error (Failure "No head")

let len p =
  let rec go total p =
    match next p with
    | Some (_, p') -> go (total + 1) p'
    | None -> total in
  go 0 p

let len p =
  let rec go total p =
    match next p with
    | Some (_, p') -> go (total + 1) p'
    | None -> total in
  go 0 p

let last p =
  let rec loop last_opt p =
    match next p with
    | Some (a, p') -> loop (Some a) p'
    | None         -> last_opt in
  loop None p

let sum p = fold (+) 0 p

let rec any node =
  match next node with
  | Some (a, _) when a -> a
  | Some (a, node) -> any node
  | None -> false

module Opt_io = struct
  type ('a, 'b, 'r) node =
    | Ready of 'r
    | Empty
    | Yield of ('b  * ('a, 'b, 'r) node)
    | Await of ('a option -> ('a, 'b, 'r) node)

  let return x = Ready x

  let rec (>>=) n f =
    match n with
    | Empty         -> Empty
    | Ready r       -> f r
    | Yield (o, n') -> Yield (o, n' >>= f)
    | Await k       -> Await (fun i -> k i >>= f)

  let rec compose s1 s2 =
    match s1, s2 with
    | Empty           , Empty           -> Empty
    | Empty           , Ready r2        -> Empty
    | Empty           , Yield (b2, s2') -> Empty
    | Empty           , Await k2        -> Empty

    | Ready r1        , Empty           -> Ready r1
    | Ready r1        , Ready r2        -> Ready r1
    | Ready r1        , Yield (b, s2')  -> Ready r1
    | Ready r1        , Await k2        -> Ready r1

    | Yield (b1, s1') , Empty           -> Yield (b1, compose s1' s2)
    | Yield (b1, s1') , Ready r2        -> Yield (b1, compose s1' s2)
    | Yield (b1, s1') , Yield (b2, s2') -> Yield (b1, compose s1' s2)
    | Yield (b1, s1') , Await k2        -> Yield (b1, compose s1' s2)

    | Await k1        , Yield (b2, s2') -> compose (k1 (Some b2)) s2'
    | Await k1        , Await k2        -> Await (fun a -> compose s1 (k2 a))
    | Await k1        , Ready r2        -> Ready r2
    | Await k1        , Empty           -> Empty

  let (<=) s1 s2 = compose s1 s2
  let (=>) s2 s1 = compose s1 s2

  let run s =
    match s with
    | Ready r -> Some r
    | Empty -> None
    | _ -> fail "Stream not ready to be run"

  let rec of_list xs =
    match xs with
    | x::xs' -> Yield (x, of_list xs')
    | []     -> Empty

  let rec take n =
    if n = 0 then Empty
    else Await begin function
        | Some i -> Yield (i, take (n - 1))
        | None -> Empty
      end

  let rec map f =
    Await (function Some a -> Yield (f a, map f)
                  | None -> Empty)

  let sum =
    let rec loop total =
      Await begin function
        | None -> Ready total
        | Some i -> loop (total + i)
      end in loop 0

  let collect =
    let rec go acc =
      Await begin function
        | Some i -> go (i::acc)
        | None   -> Ready (List.rev acc)
      end in go []
end


module Req_test = struct

  (* Internal *)
  type req = Continue | Terminate

  let request () = fail "to do"

  (* Producers *)

  (* Yields the elements of `xs` into the flow. Terminates when the list is
     empty. If the downnode signals  *)
  let rec read_detections_from_file file_path ctl =
    let chan = open_in file_path in
    let rec loop chan =
      match ctl with
      | Continue ->
        begin match catch input_line chan with
          | Ok det  -> yield det >> loop chan
          | Error _ -> empty
        end
      | Terminate -> close_in chan; empty in
    loop chan

  let encode_detections () =
    request () >>= fun det ->
    return ()

  (* Consumers *)

  let nth n =
    if n < 0 then fail "negative index";
    let rec loop n = lazy begin
      Await (function
          | Some i ->
            if n = 0 then return i
            else loop (n - 1)
          | None -> fail "premature source termination" )
    end in loop n

  let nth n =
    if n < 0 then fail "negative index";
    let rec loop n =
      await >>= function
      | Some i ->
        if n = 0 then return i
        else loop (n - 1)
      | None -> fail "premature source termination" in
    loop n

  let nth n =
    if n < 0 then fail "negative index";
    let rec loop n =
      await >>= function
      | Some i ->
        if n = 0 then return i
        else loop (n - 1)
      | None -> fail "premature source termination" in
    loop n

  (* Random *)

  let rec get_line_from_file file_path status =
    let chan = open_in file_path in
    let rec loop chan status =
      try lazy (Yield (Some (input_line chan), loop chan status))
      with End_of_file -> close_in chan; return None in
    loop chan status

  (* imaginative *)
  let rec get_line_from_file file_path status =
    let chan = open_in file_path in
    let rec loop chan status =
      match status with
      | `Continue ->
        try lazy (Yield (input_line chan, loop chan status))
        with End_of_file -> close_in chan; empty in
    loop chan status

  let rec source_list xs = function
    | `Resquest x -> `Not_yet
    | `Continue -> `Not_yet
end


(*
 * Divices
 *)

(* let rec mux t = *)
(* match t with *)
(* | lazy (Yield (o, t') -> (lazy (Yield (o, t'), lazy (Yield (o, t')) *)
(* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
(* match t1, t2 with *)
(* | lazy (Yield (o1, t1'), lazy (Yield (o2, t2') -> lazy (Yield ((o1, o2), merge t1' t2') *)
(* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)


open Opt_io

