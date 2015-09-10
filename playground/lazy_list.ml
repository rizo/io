
type 'a node =
    | Nil
    | Cons of 'a * 'a lazy_list
and
  'a lazy_list = 'a node lazy_t

let rec map f l =
  lazy begin match Lazy.force l with
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
  end

let rec take n l =
  lazy begin
    if n = 0 then Nil
    else match Lazy.force l with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (x, (take (n - 1) xs))
  end


