
- yield and await cannot be used directly for now, they must be macros.

```
Strict task definition
let rec compose_strict t1' t2' =
 match (t1', t2') with
 | Yield (x1, t1), t2             -> yield x1 >> (t1 <-< t2)
 | Ready r1      , _              -> Ready r1
 | Await k1      , Yield (x2, t2) -> k1 x2 <-< t2
 | t1            , Await k2       -> await >>= fun x -> t1 <-< k2 x
 | _             , Ready r2       -> Ready r2

and (<-<) t1 t2 = compose_strict t1 t2
and (>->) t1 t2 = compose_strict t2 t1
```


---

=> count

Yield (0,
    Yield (1,
        Yield (2, ...)))))

=> map f

Await (a1 => Yield (f a1,
    Await (a2 => Yield (f a2,
        Await (a3 => Yield (f a3, ...))))))

=> take 3

Await (a1 => Yield (a1,
    Await (a2 => Yield (a2,
        Await (a3 => Close)))))


=> nth 3

Await (a1 =>
    Await (a2 =>
        Await (a3 => Value a3)))


=> collect

Await (a1 => Yield (a1,
    Await (a2 => Yield (a2,
        Await (a3 =>
            ...
                Await (an => Value [a1; a2; a3; ... ; an])))))

- Iteratees in the current configuration of the framework have no way to find
  out if a task is closed of finished its computation.

- Await action
