(* let () = print_endline "Hello, World!" *)


open Async
open Core
open Base

let dequeue_and_print q = (
    match Queue.dequeue q with
    | Some s -> Stdio.print_endline s
    | None -> Stdio.print_endline "None"
  );
  q

let run (): unit =
  Stdio.print_endline "Start" ;
  let _ = return (Queue.of_list [])
  >>| (fun q -> Queue.enqueue q "alpha" ; q)
  >>| (fun q -> Queue.enqueue q "beta" ; q)
  >>| dequeue_and_print
  >>| (fun q -> Queue.enqueue q "gamma" ; q)
  >>| dequeue_and_print
  >>| dequeue_and_print
  >>| (fun _ -> ()) in
  shutdown 0
  
let () =
  run () ;
  never_returns (Scheduler.go ())
