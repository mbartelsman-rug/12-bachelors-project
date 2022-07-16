open Interpreter.Interpreter

let select_thread env = 
  let (_, tpool, ch, fn) = env in
  let* (tpool', (expr, wait)) = queue_dequeue tpool in
  let env' = (wait, tpool', ch, fn) in
  M.ret (env', expr)


let schedule_thread (env, thread) =
  let (wait, tpool, ch, fn) = env in
  let* tpool' = queue_enqueue (tpool, (thread, wait)) in
  let env' = (False, tpool', ch, fn) in
  M.ret env'


let rec print_threads' tlist tstr =
  match tlist with
  | (thd, True) :: ttl -> print_threads' ttl (tstr ^ "||: " ^ (string_of_expr thd) ^ "\n")
  | (thd, False) :: ttl -> print_threads' ttl (tstr ^ "||> " ^ (string_of_expr thd) ^ "\n")
  | [] -> tstr
  

let print_threads env =
  let open Base in
  let (_, tpool, _, _) = env in
  let tlist = Queue.to_list tpool in
  print_threads' tlist ""


let check_all_threads env = 
  let open Base in
  let (_, tpool, _, _) = env in
  begin match Queue.is_empty tpool with
  | true -> `Finished
  | false -> (
    let all_waiting = List.for_all
      (Queue.to_list tpool)
      ~f:(fun (_, waiting) -> unmake_bool waiting) in
    begin match all_waiting with
    | false -> `Running
    | true -> `Livelock
    end
  )
  end


let check_thread expr =
  match expr with
  | Ret _ -> `Finished
  | _ -> `NotFinished


let rec evaluate' env results print =
  let* (env, expr) = select_thread env in
  let* (env', expr') = expr_reduce (env, expr) in
  let* (env'', results') = 
  begin match check_thread expr' with
  | `Finished ->
    Stdio.print_endline (string_of_expr expr') ;
    M.ret (env', expr' :: results)
  | `NotFinished -> (
    let* env'' = schedule_thread (env', expr') in
    M.ret (env'', results)
  )
  end in
  begin match print with
    | true -> Stdio.print_endline (print_threads env)
    | _ -> ()
  end ;
  begin match check_all_threads env'' with
  | `Finished -> results' |> M.ret
  | `Livelock -> results' |> M.ret
  | `Running -> evaluate' env'' results' print
  end


let evaluate ?(print = false) expr =
  let env = make_env () in
  ( let* env' = env_fork (env, expr) in
    let results = [] in
    evaluate' env' results print
  ) |> M.extract

let parse (strn: string): expr_t =
  expr_of_string strn

let serialize (expr: expr_t): string =
  string_of_expr expr


let run (program: string): string =
  parse program
  |> evaluate
  |> Base.List.map ~f:(serialize)
  |> Base.String.concat ~sep:("\n||")
  