open Interpreter.Interpreter


let select_actor env = 
  let (_, tpool, ch, fn) = env in
  let* (tpool', (pid, expr)) = queue_dequeue tpool in
  let env' = (pid, tpool', ch, fn) in
  M.ret (env', pid, expr)


let schedule_actor (env, thread) =
  let ((pid, wait), tpool, ch, fn) = env in
  let* tpool' = queue_enqueue (tpool, ((pid, wait), thread)) in
  let env' = (("", False), tpool', ch, fn) in
  M.ret env'


let rec print_threads' tlist tstr =
  begin match tlist with
  | ((pid, True), thd) :: ttl -> print_threads' ttl (tstr ^ "||" ^ pid ^ " :: " ^ (string_of_expr thd) ^ "\n")
  | ((pid, False), thd) :: ttl -> print_threads' ttl (tstr ^ "||" ^ pid ^ " :> " ^ (string_of_expr thd) ^ "\n")
  | [] -> tstr
  end
  

let print_threads env =
  let open Base in
  let (_, tpool, _, _) = env in
  let tlist = Queue.to_list tpool in
  print_threads' tlist "\n"


let check_all_actors env locked = 
  let open Base in
  let (_, tpool, _, _) = env in
  let empty = Queue.is_empty tpool in
  begin match empty with
  | true -> `Finished
  | false -> (
    let all_waiting = List.for_all
      (Queue.to_list tpool)
      ~f:(fun ((_, waiting), _) -> unmake_bool waiting) in
      begin match all_waiting with
      | false -> `Running
      | true -> 
        (* Hack to make sure all threads are evaluated at least once before
           declaring a livelock situation *)
        begin match locked with
        | `Locked num -> `Livelock (num - 1)
        | `NotLocked -> `Livelock (Base.Queue.length tpool)
        end
      end
  )
  end


let check_actor expr =
  begin match expr with
  | Ret _ -> `Finished
  | _ -> `NotFinished
  end


let rec evaluate' env results locked print =
  let* (env, pid, expr) = select_actor env in
  let* (env', expr') = expr_reduce (env, expr) in
  let* (env'', results') = 
  begin match check_actor expr' with
  | `Finished    ->
    Stdio.print_endline ((unmake_name (fst pid)) ^ ": " ^ (string_of_expr expr')) ;
    M.ret (env', (pid, expr') :: results)
  | `NotFinished ->
    let* env'' = schedule_actor (env', expr') in
    M.ret (env'', results)
  end in
  begin match print with
  | true  -> Stdio.print_endline (print_threads env)
  | false -> ()
  end ;
  begin match check_all_actors env'' locked with
  | `Finished -> results' |> M.ret
  | `Livelock 0 -> results' |> M.ret
  | `Livelock n -> evaluate' env'' results' (`Locked n) print
  | `Running -> evaluate' env'' results' `NotLocked print
  end


let evaluate_with_pids ?(print = false) expr =
  let env = make_env () in
  ( let* (env', _) = actor_spawn (env, expr) in
    evaluate' env' [] `NotLocked print
  ) |> M.extract


let evaluate ?(print = false) expr =
  evaluate_with_pids expr ~print:print
  |> Base.List.map ~f:(fun ((pid,_), expr) -> (pid, expr))
  |> Base.List.sort ~compare:(fun (a,_) (b,_) -> Base.String.compare a b)


let parse (strn: string): expr_t =
  expr_of_string strn


let serialize (expr: expr_t): string =
  string_of_expr expr


let run (program: string): string =
  parse program
  |> evaluate ~print:(false)
  |> Base.List.map ~f:(fun (pid, expr) -> (unmake_name pid) ^ ": " ^ (serialize expr))
  |> Base.String.concat ~sep:("\n||")
