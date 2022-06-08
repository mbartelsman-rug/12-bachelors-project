module Interpreter: sig
  include Interpreter.TYPES
  module M: Common.Monads.MONAD

  val evaluate: ?print:bool -> (expr_t) -> (env_t)

end = struct
  include Interpreter.Interpreter

  let select_thread env = 
    let (tpool, ch, fn) = env in
    let* (tpool', expr) = queue_dequeue tpool in
    let env' = (tpool', ch, fn) in
    M.ret (env', expr)
  

  let schedule_thread (env, thread) =
    let (tpool, ch, fn) = env in
    let* tpool' = queue_enqueue (tpool, thread) in
    let env' = (tpool', ch, fn) in
    M.ret env'


  let compare_state env1 env2 =
    let (tpool1, _, _) = env1 in
    let (tpool2, _, _) = env2 in
    match tpool1 = tpool2 with
    | true -> `Eq
    | false -> `Diff


  let rec print_threads' tlist tstr =
    match tlist with
    | thd :: ttl -> print_threads' ttl (tstr ^ "\n|| " ^ (string_of_expr thd))
    | [] -> tstr
    

  let print_threads env =
    let open Base in
    let (tpool, _, _) = env in
    let tlist = Queue.to_list tpool in
    print_threads' tlist "|| "


  let rec evaluate' env print =
    let* (env, expr) = select_thread env in
    let* (env', expr') = expr_reduce (env, expr) in
    let* env'' = schedule_thread (env', expr') in

    ( match print with
      | true -> Stdio.print_endline (print_threads env)
      | _ -> ()
    ) ;

    match compare_state env env'' with
    | `Eq -> M.ret env
    | `Diff -> evaluate' env'' print


  let evaluate ?(print = false) expr =
    let env = make_env () in (
      let* env' = env_fork (env, expr) in
      evaluate' env' print
    ) |> M.extract
end

(* 
  start with an expr and empty env
  shove expr into thread
  execute
*)
