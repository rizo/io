

open Elements

type ('a, 'b) task = unit -> ('a, 'b) task_status
 and ('a, 'b) task_status =
   | Ready
   | Yield of ('b *  ('a, 'b) task)
   | Await of ('a -> ('a, 'b) task)

type 'o producer = (unit, 'o  ) task
type 'i consumer = ('i  , unit) task

let zero =
  fun () -> Ready

let return x =
  fun () -> Yield (x, zero)

let rec (>>=) t f =
  match t () with
  | Ready                            -> fun () -> Ready
  | Yield (o, t') when t' () = Ready -> f o
  | Yield (o, t')                    -> fun () -> Yield (o, t' >>= f)
  | Await k                          -> fun () -> Await (fun i -> k i >>= f)

let (>>) t1 t2 =
  t1 >>= fun () -> t2

let yield b =
  fun () -> Yield (b, zero)

let await =
  fun () -> Await (fun i () -> Yield (i, zero))

let rec (<-<) t1 t2 =
  match t1 (), t2 () with
  | Ready          , _              -> fun () -> Ready
  | Yield (b, t1') , t2'            -> fun () -> Yield (b, t1' <-< t2)
  | Await f        , Yield (b, t2') -> f b <-< t2'
  | t1'            , Await f        -> fun () -> Await (fun a -> t1 <-< f a)
  | _              , Ready          -> fun () -> Ready

let rec cat =
  fun () ->
    Await (fun a -> fun () -> Yield (a, cat))

let rec run t =
  match t () with
  | Ready                            -> None
  | Yield (o, t') when t' () = Ready -> Some o
  | Yield (o, t')                    -> run t'
  | Await k                          -> run (k ())

let rec repeat a =
  fun () -> Yield (a, repeat a)

let rec repeatedly f =
  fun () -> Yield (f (), repeatedly f)

let rec discard =
  fun () -> Await (fun i -> discard)

let rec yes = repeat "y"
let rec  no = repeat "n"

let rec count =
  let rec loop n =
    fun () -> Yield (n, loop (n + 1)) in
  loop 0

let nth n =
  let rec loop i () =
    Await begin fun x ->
      if i = n then fun () -> Yield (x, zero)
      else loop (i + 1)
    end
  in loop 0

let rec take n =
  if n = 0 then zero
  else fun () -> Await (fun i -> fun () -> Yield (i, take (n - 1)))

let rec map f =
  fun () -> Await (fun i -> fun () -> Yield (f i, map f))

let rec print =
  fun () -> Await (fun i -> print_endline i; print)

let rec of_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> fun () -> Yield (line, of_chan ch)
  | None -> zero

let to_list producer =
  let rec go acc t =
    match t() with
    | Ready -> acc
    | Yield (o, t') -> go (o::acc) t'
    | Await k       -> failwith "should be closed"
  in go [] producer

(* toList :: Producer a Identity () -> [a]    *)
(* toList = go                                *)
(*   where                                    *)
(*     go p = case p of                       *)
(*         Request v _  -> closed v           *)
(*         Respond a fu -> a:go (fu ())       *)
(*         M         m  -> go (runIdentity m) *)
(*         Pure    _    -> []                 *)

(* let rec mux t = *)
  (* match t with *)
  (* | Yield (o, t') -> (Yield (o, t'), Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | Yield (o1, t1'), Yield (o2, t2') -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



