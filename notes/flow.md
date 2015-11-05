
```
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
```
