(* TEST
* hasunix
include unix
** bytecode
** native
*)

let percent_finalize = try int_of_string Sys.argv.(1) with _ -> 50
let iterations = try int_of_string Sys.argv.(2) with _ -> 1000
let num_domains = try int_of_string Sys.argv.(3) with _ -> 4

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r

let () = Random.init 42
let n = 10

let allocate () =
  for _ = 0 to 1000 do
    let v = make n in
    if Random.int 100 < percent_finalize then
      Gc.finalise (fun v -> ignore @@ check v) v
    else
      ignore @@ check v
  done

let work () =
  for _ = 0 to iterations do
    allocate()
  done

let _ =
  let domains = Array.init (num_domains - 1) (fun _ -> Domain.spawn(work)) in
  work ();
  Array.iter Domain.join domains
