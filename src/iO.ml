
open Elements

type ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node =
  | Client of ('a_req  * ('a_rep -> ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node))
  | Server of ('b_req -> ('b_rep  * ('a_req, 'a_rep, 'b_req, 'b_rep, 'r) node))
  | Result of 'r

let return x = Result x

let rec (>>=) n0 f =
  let rec go n =
    match n with
    | Client (req, h) -> Client (req, fun rep -> go (h req))
    | Server k        -> Server (fun req -> let rep, n' = k req in (rep, go n'))
    | Result r        -> f r in
  go n0

let (>>) s1 s2 =
  s1 >>= fun _ -> s2

let reply   a = Server (fun req -> a, Result ())
let request a = Client (a, fun rep -> Result rep)

let yield a = Server (fun () -> a, Result None)
let await   = Client ((), fun rep -> Result (Some rep))

let rec (>>>) n1 n2 =
  match n1, n2 with
  | _, Result r               -> Result r
  | _, Server k               -> Server (fun req -> let rep, n' = k req in (rep, n1 >>> n'))
  | Server k, Client (req, h) -> let rep, n' = k req in n' >>> h rep
  | Client (req, h), Client _ -> Client (req, fun rep -> h rep >>> n2)
  | Result r       , Client _ -> Result r

let (<<<) n2 n1 = n1 >>> n2

let rec run n =
  match n with
  | Result r        -> r
  | Server k        -> let (_rep, n') = k () in run n'
  | Client (req, h) -> run (h ())


