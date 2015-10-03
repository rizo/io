

open Elements

type ('a, 'b, 'r) task = unit -> ('a, 'b, 'r) task_status
 and ('a, 'b, 'r) task_status =
   | Ready of 'r
   | Yield of ('b *  ('a, 'b, 'r) task)
   | Await of ('a -> ('a, 'b, 'r) task)

type ('o, 'a) producer = (unit, 'o  , 'a) task
type ('i, 'a) consumer = ('i  , unit, 'a) task
type      'a  workflow = (unit, unit, 'a) task

let zero =
  fun () -> Ready ()

let return x =
  fun () -> Yield (x, zero)

let rec (>>=) t f =
  match t () with
  | Ready a       -> f a
  | Yield (o, t') -> fun () -> Yield (o, t' >>= f)
  | Await k       -> fun () -> Await (fun i -> k i >>= f)

let (>>) t1 t2 =
  t1 >>= fun () -> t2

let yield b =
  fun () -> Yield (b, zero)

let await =
  fun () -> Await (fun i () -> Yield (i, zero))

let rec (<-<) t1 t2 =
  match t1 (), t2 () with
  | Ready a        , _              -> fun () -> Ready a
  | Yield (b, t1') , t2'            -> fun () -> Yield (b, t1' <-< t2)
  | Await f        , Yield (b, t2') -> f b <-< t2'
  | t1'            , Await f        -> fun () -> Await (fun a -> t1 <-< f a)
  | _              , Ready a        -> fun () -> Ready a

let rec cat =
  fun () ->
    Await (fun a -> fun () -> Yield (a, cat))

let rec run t =
  match t () with
  | Ready r       -> r
  | Yield (Void, b) -> run b
  | Await k       -> run (k ())

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

(* let rec mux t = *)
  (* match t with *)
  (* | Yield (o, t') -> (Yield (o, t'), Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | Yield (o1, t1'), Yield (o2, t2') -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



