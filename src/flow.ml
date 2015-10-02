

open Elements

type ('i, 'o, 'a) task = unit -> ('i, 'o, 'a) task_status
and ('i, 'o, 'a) task_status =
  | Ready of 'a
  | Yield of ('o *  ('i, 'o, 'a) task)
  | Await of ('i -> ('i, 'o, 'a) task)

(* type ('o, 'a) producer = (unit, 'o  , 'a) task *)
(* type ('i, 'a) consumer = ('i  , unit, 'a) task *)
(* type      'a  workflow = (unit, unit, 'a) task *)

let zero = fun () -> Ready ()

let _return x = fun () -> Ready x

let rec (>>=) t f =
  match t () with
  | Ready x       -> f x
  | Yield (o, t') -> fun () -> Yield (o, t' >>= f)
  | Await k       -> fun () -> Await (fun i -> k i >>= f)

let (>>) a1 a2 =
  a1 >>= fun () -> a2

let _yield x = fun () -> Yield (x, zero)

let _await = fun () -> Await (fun i () -> Ready i)

let rec run t =
  match t () with
  | Ready r       -> r
  | Await k       -> run (k ())
  | Yield ((), b) -> run b

let rec compose_strict t1 t2 =
  match t1 (), t2 () with
  | Yield (x1, t1'), t2'             -> fun () -> Yield (x1, (t1' <-< t2))
  | Ready r1       , _               -> fun () -> Ready r1
  | Await k1       , Yield (x2, t2') -> k1 x2 <-< t2'
  | t1'            , Await k2        -> fun () -> Await (fun x -> t1 <-< k2 x)
  | _              , Ready r2        -> fun () -> Ready r2

and (<-<) t1 t2 = compose_strict t1 t2
and (>->) t1 t2 = compose_strict t2 t1

(* Monad composition *)
let rec forever a = a >> forever a

let rec discard =
  fun () -> Await (fun i -> discard)

let rec yes =
  fun () -> Yield ("y", yes)

let rec no =
  fun () -> Yield ("n", no)

let nth n =
  let rec loop i () =
    Await begin fun x ->
      if i = n then fun () -> Ready x
      else loop (i + 1)
    end
  in loop 0

let rec take n () =
  if n = 0 then Ready ()
  else Await (fun i -> fun () -> Yield (i, take (n - 1)))

let rec map f =
  fun () -> Await (fun x -> fun () -> Yield (f x, map f))

let rec of_chan ch =
  match Exn.as_option End_of_file input_line ch with
  | Some line -> fun () -> Yield (line, of_chan ch)
  | None -> zero



