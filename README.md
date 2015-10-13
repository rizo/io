# OCaml Iter

(_Unreleased_)

Simple, secure and composable abstraction for efficient stream processing in a purely
functional fashion.

This library implements the _iteratees_ style of data processing. Iteratees
address the problem of doing incremental, composable I/O without being lazy.

## Examples

```ocaml
(* Compute the sum of all odd integers up to 1000000. *)
assert (iota 1000000 =>= filter odd => fold (+) 0 = 250000000000);

(* Take 5 integers from an infinit sequence and collect them into a list. *)
assert (count =>= take 5 => collect = [1; 2; 3; 4; 5]);
```
