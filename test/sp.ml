
open Elements
open Flow


let check_client (lazy client) =
  match client with
  | Yield (request, (lazy (Await handle_rep))) -> true
  | _ -> false

let rec check_server (lazy server) req =
  match server with
  | Await recv_req -> begin
      match recv_req req with
      | lazy (Yield (rep, k)) -> true
      | _ -> false
    end
  | _ -> false


let reqrep (lazy server) (lazy client) =
  match server, client with
  | Await recv_req, Yield (req, (lazy (Await handle_rep))) -> begin
      match recv_req req with
      | lazy (Yield (rep, server')) -> handle_rep rep
      | _ -> fail "reqrep: internal server error"
    end
  | _ -> fail "reqrep: bad server client setup"


module Pipeline = struct
  let rec node0 = lazy begin
    Await (fun msg -> print (fmt "node0: received \"%s\"" msg); node0)
  end

  let node1 msg = lazy begin
    Yield (print (fmt "node1: sending \"%s\"" msg); msg, return ())
  end

  let test () =
    run (node1 "Hello, World!" >> node1 "Goodbye." => node0)
end


module Reqrep = struct

  let request req =
    lazy (Yield (req, lazy (Await return)))

  let rec server : (string, float option, 'a option) flow =
    Flow.map begin function
      | "date"  -> Some (Unix.gettimeofday ())
      | unknown -> None
    end

  let client = request
end

