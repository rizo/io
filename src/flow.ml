

open Elements

 type ('i, 'o) stream =
   | Empty
   | Yield of ('o  * ('i, 'o) stream)
   | Await of ('i -> ('i, 'o) stream)

let return x =
  Yield (x, Empty)

let rec (<-<) t1 t2 =
  match t1, t2 with
  | Empty         , _             -> Empty
  | Yield (b, t') , _             -> Yield (b, t' <-< t2)
  | Await k       , Yield (b, t') -> k b <-< t'
  | _             , Await k       -> Await (fun a -> t1 <-< k a)
  | _             , Empty         -> Empty

let (>->) t2 t1 = t1 <-< t2


let rec (>>=) a f =
  match a with
  | Empty         -> Empty
  | Yield (x, a') -> f x >-> (a' >>= f)
  | Await k       -> Await (fun x -> k x >>= f)


let (>>) t1 t2 =
  t1 >>= fun () -> t2


(* type ('i, 'o) action = unit -> ('i, 'o) status *)
(*  and ('i, 'o) status = *)
(*    | Close *)
(*    | Yield of ('o  * ('i, 'o) action) *)
(*    | Await of ('i -> ('i, 'o) action) *)

(* let return x = *)
(*   fun () -> Yield (x, fun () -> Close) *)

(* let rec (<-<) t1 t2 = *)
(*   match t1 (), t2 () with *)
(*   | Close         , _             -> fun () -> Close *)
(*   | Yield (b, t') , _             -> fun () -> Yield (b, t' <-< t2) *)
(*   | Await k       , Yield (b, t') -> k b <-< t' *)
(*   | _             , Await k       -> fun () -> Await (fun a -> t1 <-< k a) *)
(*   | _             , Close         -> fun () -> Close *)

(* let (>->) t2 t1 = (<-<) t1 t2 *)


(* let rec (>>=) a f = *)
(*   match a () with *)
(*   | Close         -> fun () -> Close *)
(*   | Yield (x, a') -> f x >-> (a' >>= f) *)
(*   | Await k       -> fun () -> Await (fun x -> k x >>= f) *)


(* let (>>) t1 t2 = *)
(*   t1 >>= fun () -> t2 *)

(* let _yield b = *)
(*   fun () -> Yield (b, fun () -> Close) *)

(* let _await = *)
(*   fun () -> Await (fun a -> fun () -> Yield (a, fun () -> Close)) *)


(* let rec cat = *)
(*   fun () -> *)
(*     Await (fun a -> fun () -> Yield (a, cat)) *)

(* let rec run t = *)
(*   match t () with *)
(*   | Close                            -> None *)
(*   | Yield (x, t') when t' () = Close -> Some x *)
(*   | Yield (x, t')                    -> run t' *)
(*   | Await k                          -> run (k ()) *)

(* let rec repeat a = *)
(*   fun () -> Yield (a, repeat a) *)

(* let rec repeatedly f = *)
(*   fun () -> Yield (f (), repeatedly f) *)

(* let rec discard = *)
(*   fun () -> Await (fun i -> discard) *)

(* let rec yes = repeat "y" *)
(* let rec  no = repeat "n" *)

(* let rec count = *)
(*   let rec loop n = *)
(*     fun () -> Yield (n, loop (n + 1)) in *)
(*   loop 0 *)

(* let nth n = *)
(*   let rec loop i = *)
(*     fun () -> Await (fun a -> *)
(*         if i = n *)
(*         then fun () -> Yield (a, fun () -> Close) *)
(*         else loop (i + 1)) *)
(*   in loop 0 *)

(* let rec take n = *)
(*   if n = 0 *)
(*   then fun () -> Close *)
(*   else fun () -> Await (fun i -> fun () -> Yield (i, take (n - 1))) *)

(* let rec map f = *)
(*   fun () -> Await (fun a -> fun () -> Yield (f a, map f)) *)

(* let rec print = *)
(*   fun () -> Await (fun a -> print_endline a; print) *)

(* let rec of_chan ch = *)
(*   match Exn.as_option End_of_file input_line ch with *)
(*   | Some line -> fun () -> Yield (line, of_chan ch) *)
(*   | None      -> fun () -> Close *)

(* let rec of_list xs = *)
(*   match xs with *)
(*   | x::xs' -> fun () -> Yield (x, of_list xs') *)
(*   | []     -> fun () -> Close *)

(* let to_list producer = *)
(*   let rec go acc t = *)
(*     match t () with *)
(*     | Close         -> acc *)
(*     | Yield (o, t') -> go (o::acc) t' *)
(*     | Await k       -> failwith "should be closed" *)
(*   in go [] producer *)

(* let rec mux t = *)
  (* match t with *)
  (* | Yield (o, t') -> (Yield (o, t'), Yield (o, t')) *)
  (* | _ -> failwith "not compatible" *)

(* let rec merge t1 t2 = *)
  (* match t1, t2 with *)
  (* | Yield (o1, t1'), Yield (o2, t2') -> Yield ((o1, o2), merge t1' t2') *)
  (* | _ -> failwith "not compatible" *)

(* let split t = (map fst t, map snd t);; *)



