# Flow

_Work in progress_

Simple, secure and composable abstraction for efficient stream processing.

This library implements flow-based programming primitives for data processing. An application can be viewed as a network of asynchronous processes, called _nodes_, communicating by means of streams of structured data chunks.

### Features

- Simple extandable and composable core.
- Early termination by downstream.
- Notification of uptream termination.
- Prompt finalization of resources.

## Examples

```ocaml

(* Produces a stream of integers from `start` to `stop. *)
let rec range start stop =
  count => take stop => drop start
  
(* Applies a function to each element of a stream. *)
let rec map f =
  await >>= fun a -> yield (f a) >> map f

(* Compute the sum of all odd integers up to 1000000. *)
assert (iota 1000000 => filter odd => fold (+) 0 = 250000000000);

(* Take 5 integers from an infinit sequence and collect them into a list. *)
assert (count => take 5 => collect = [1; 2; 3; 4; 5]);
```
