(* Foldable Iteratation

   Playing with folds and iters described in:
     <http://newartisans.com/2014/06/simpler-conduit-library/>
   Iteration library based on monadic folds. *)

(*sourceList :: Monad m => [b] -> a -> (a -> b -> m a) -> m a*)
(*source_list [1..10] 0 $ \acc x -> return $ acc + x*)

module Native = struct
  let rec map f xs =
    match xs with
    | [] -> []
    | x::xs' -> f x :: map f xs'
end

module Foldable = struct
  type ('a, 'r) source = 'r -> ('r -> 'a -> 'r) -> 'r

  type ('a, 'r) sink = ('a, 'r) source -> 'r

  type ('a, 'b, 'r) pipe = ('a, 'r) source -> ('b, 'r) source

  let sum await =
    await 0 (fun acc x -> acc + x)

  let length await =
    await 0 (fun total x -> total + 1)

end

module List = struct
  include List

  let rec enum xs acc f =
      match xs with
      | [] -> acc
      | x::xs' -> enum xs' (f acc x) f
end

module File = struct

  let rec enum file_path acc f =
    let in_channel = open_in file_path in
    let rec loop acc =
      try
        loop (f acc (input_line in_channel))
      with End_of_file ->
        close_in in_channel;
        acc in
    loop acc
end


module Ex1 = struct

  type 'a input =
    | Item of 'a
    | Stop

  type ('a, 'r) step =
    | Yield of 'r * 'a input
    | Await of ('a input -> ('a, 'r) step)
    | Error of exn

  type ('a, 'b, 'r) pipe =
    ('a, 'b) step -> ('b, 'r) step

  (*let rec iter_list xs current_state =*)
    (*match (current_state, xs) with*)
    (*| Await k, x::xs' -> iter_list xs' (k (Item x))*)
    (*| _ -> current_state*)

  (*let rec iter_list xs acc f =*)
    (*match xs with*)
    (*| [] -> acc*)
    (*| x::xs' -> iter_list xs' (f acc x) f*)

  (*let rec map f input =*)
    (*match input with*)
    (*| `Empty -> `Empty*)
    (*| `Yield (x, input') -> `Yield (f x, map f input')*)

  (*let run step =*)
    (*match step with*)
    (*| Yield (x, _) -> Some x*)
    (*| Continue k ->*)
      (*begin match (k End) with*)
        (*| Yield (x, _) -> Some x*)
        (*| _ -> None*)
      (*end*)
    (*| Error _ -> None*)

end

open Ex1


module ExPipes = struct
  (*  _bind                                                  *)
  (*    :: Monad m                                           *)
  (*    => Proxy a' a b' b m r                               *)
  (*    -> (r -> Proxy a' a b' b m r')                       *)
  (*    -> Proxy a' a b' b m r'                              *)
  (*p0 `_bind` f = go p0 where                               *)
  (*    go p = case p of                                     *)
  (*        Request a' fa  -> Request a' (\a  -> go (fa  a ))*)
  (*        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))*)
  (*        M          m   -> M (m >>= \p' -> return (go p'))*)
  (*        Pure    r      -> f r                            *)

  let bind iter f =
    let rec go = function
      | Await k          -> Await (fun input -> go (k input))
      | Yield (r, input) -> Yield (f r, input)
      | Error e          -> Error e
    in
      go iter

end

(* mapI :: Monad m => (ao -> ai) -> Enumeratee e ao ai m b *)
(* mapI f = checkDone $ continue . step where              *)
(*  step k EOF = yield (Continue k) EOF                    *)
(*  step k (Chunk []) = continue $ step k                  *)
(*  step k chunk = k (fmap f chunk) >>== mapI f            *)



(* let rec take n iter =                                                                            *)
(*   if n = 0 then (print_endline "0";iter)                                                         *)
(*   else match iter with                                                                           *)
(*     | Await k -> print_endline "a";Await (fun input -> print_endline "i";take (n - 1) (k input)) *)
(*     | Yield (r, _) -> print_endline "y";iter                                                     *)
(*     [>| Yield (r, _) -> drop n >> iter<]                                                         *)
(*     | Error e -> Error e                                                                         *)



value (map_stream : ('elo -> 'eli) -> enumeratee 'elo 'eli 'a) f i =
  let rec map_stream i =
    match i with
    [ IE_cont None k -> ie_cont (step k)
    | IE_cont (Some _) _ | IE_done _ -> return i
    ]
  and step k s =
    match s with
    [ Chunk c ->
        if S.is_empty c
        then ie_contM (step k)
        else
          k (Chunk (S.map f c)) >>% fun (iv, _) ->
          IO.return (map_stream iv, Sl.empty)
    | EOF _ ->
        ie_doneM (ie_cont k) s
    ]
  in
    map_stream i

