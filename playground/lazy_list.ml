
open Elements

type 'a node =
    | Nil
    | Cons of 'a * 'a lazy_list
and
  'a lazy_list = 'a node lazy_t

let rec map f ll =
  lazy begin match Lazy.force ll with
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
  end

let rec take n ll =
  lazy begin
    if n = 0 then Nil
    else match Lazy.force ll with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (x, (take (n - 1) xs))
  end

let rec len ll =
  let rec loop total ll =
    match Lazy.force ll with
    | Nil -> total
    | Cons (_, xs) -> loop (total + 1) xs
  in
    loop 0 ll

let rec of_list l =
  lazy begin match l with
    | [] -> Nil
    | x :: xs -> Cons (x, of_list xs)
  end

let rec to_list ll =
  match Lazy.force ll with
  | Nil -> []
  | Cons (x, xs) -> x :: to_list xs

let rec of_chan ch =
  lazy begin match Exn.as_option End_of_file input_line ch with
    | Some line -> Cons (line, of_chan ch)
    | None -> Nil
  end

