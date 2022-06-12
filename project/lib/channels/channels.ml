module Interpreter: sig
  include Interpreter.TYPES
  module M: Common.Monads.MONAD

  val evaluate: ?print:bool -> (expr_t) -> (expr_t list)

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


  let rec print_threads' tlist tstr =
    match tlist with
    | thd :: ttl -> print_threads' ttl (tstr ^ "|| " ^ (string_of_expr thd) ^ "\n")
    | [] -> tstr
    

  let print_threads env =
    let open Base in
    let (tpool, _, _) = env in
    let tlist = Queue.to_list tpool in
    print_threads' tlist "\n"


  let check_all_threads env = 
    let open Base in
    let (tpool, _, _) = env in
    match Queue.is_empty tpool with
    | true -> `Empty
    | false -> `NotEmpty


  let check_thread expr =
    match expr with
    | Ret _ -> `Finished
    | _ -> `NotFinished


  let rec evaluate' env results print =
    let* (env, expr) = select_thread env in
    let* (env', expr') = expr_reduce (env, expr) in
    let* (env'', results') = 
    ( match check_thread expr' with
    | `Finished -> M.ret (env', expr' :: results)
    | `NotFinished ->
      ( let* env'' = schedule_thread (env', expr') in
        M.ret (env'', results)
      )
    ) in
    ( match print with
      | true -> Stdio.print_endline (print_threads env)
      | _ -> ()
    ) ;
    ( match check_all_threads env'' with
    | `Empty -> results' |> M.ret
    | `NotEmpty -> evaluate' env'' results' print
    )


  let evaluate ?(print = false) expr =
    let env = make_env () in
    ( let* env' = env_fork (env, expr) in
      let results = [] in
      evaluate' env' results print
    ) |> M.extract
end

(* 
  start with an expr and empty env
  shove expr into thread
  execute
*)
