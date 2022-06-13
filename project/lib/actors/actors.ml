module Interpreter: sig
  include Interpreter.TYPES
  module M: Common.Monads.MONAD

  val evaluate: ?print:bool -> (expr_t) -> ((pid_t * expr_t) list)

end = struct
  include Interpreter.Interpreter

  let select_actor env = 
    let (_, tpool, ch, fn) = env in
    let* (tpool', (pid, expr)) = queue_dequeue tpool in
    let env' = (pid, tpool', ch, fn) in
    M.ret (env', pid, expr)
  

  let schedule_actor (env, thread) =
    let (pid, tpool, ch, fn) = env in
    let* tpool' = queue_enqueue (tpool, (pid, thread)) in
    let env' = ("", tpool', ch, fn) in
    M.ret env'


  let rec print_threads' tlist tstr =
    begin match tlist with
    | (pid, thd) :: ttl -> print_threads' ttl (tstr ^ "||" ^ pid ^ "::" ^ (string_of_expr thd) ^ "\n")
    | [] -> tstr
    end
    

  let print_threads env =
    let open Base in
    let (_, tpool, _, _) = env in
    let tlist = Queue.to_list tpool in
    print_threads' tlist "\n"


  let check_all_actors env = 
    let open Base in
    let (_, tpool, _, _) = env in
    begin match Queue.is_empty tpool with
    | true -> `Empty
    | false -> `NotEmpty
    end


  let check_actor expr =
    begin match expr with
    | Ret _ -> `Finished
    | _ -> `NotFinished
    end


  let rec evaluate' env results print =
    let* (env, pid, expr) = select_actor env in
    let* (env', expr') = expr_reduce (env, expr) in
    let* (env'', results') = 
    begin match check_actor expr' with
    | `Finished    -> M.ret (env', (pid, expr') :: results)
    | `NotFinished ->
      let* env'' = schedule_actor (env', expr') in
      M.ret (env'', results)
    end in
    begin match print with
    | true  -> Stdio.print_endline (print_threads env)
    | false -> ()
    end ;
    begin match check_all_actors env'' with
    | `Empty -> results' |> M.ret
    | `NotEmpty -> evaluate' env'' results' print
    end


  let evaluate ?(print = false) expr =
    let env = make_env () in
    ( let* (env', _) = actor_spawn (env, expr) in
      evaluate' env' [] print
    ) |> M.extract
end
