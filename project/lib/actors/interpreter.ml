module type TYPES = sig
  type _ dict_t
  and int_t
  and _ queue_t
  and string_t
  and value_t =
  | UnitVal
  | RecFuncVal of rec_func_t
  | PidVal of string_t
  | PairVal of pair_t
  | IntVal of int_t
  | FuncVal of func_t
  | EitherVal of either_t
  and expr_t =
  | Var of string_t
  | Sub of (expr_t * expr_t)
  | Spawn of expr_t
  | Snd of expr_t
  | Seq of (expr_t * expr_t)
  | Send of (expr_t * expr_t)
  | Self
  | Right of expr_t
  | Ret of value_t
  | Receive
  | RecFunc of (string_t * string_t * expr_t)
  | Pair of (expr_t * expr_t)
  | Neg of expr_t
  | Mul of (expr_t * expr_t)
  | Match of (expr_t * expr_t * expr_t)
  | Let of (string_t * expr_t * expr_t)
  | Left of expr_t
  | Func of (string_t * expr_t)
  | Fst of expr_t
  | Div of (expr_t * expr_t)
  | Call of (expr_t * expr_t)
  | Add of (expr_t * expr_t)
  and either_t =
  | RightVal of value_t
  | LeftVal of value_t
  and bool_t =
  | True
  | False
  and act_env_t = (value_t queue_t) dict_t
  and env_t = string_t * (string_t * expr_t) queue_t * (value_t queue_t) dict_t * value_t dict_t
  and func_env_t = value_t dict_t
  and func_t = string_t * expr_t
  and inbox_t = value_t queue_t
  and pair_t = value_t * value_t
  and rec_func_t = string_t * string_t * expr_t
  and thread_pool_t = (string_t * expr_t) queue_t

  val make_env: unit -> env_t
  val make_int: int -> value_t
  val make_bool: bool -> bool_t
  val make_name: string -> string_t
end

module Types = struct
  open Base

  type int_t = Int.t
  and string_t = String.t
  and 'v dict_t = 'v Map.M(String).t
  and 'v queue_t = 'v Queue.t
  and value_t =
  | UnitVal
  | RecFuncVal of rec_func_t
  | PidVal of string_t
  | PairVal of pair_t
  | IntVal of int_t
  | FuncVal of func_t
  | EitherVal of either_t
  and expr_t =
  | Var of string_t
  | Sub of (expr_t * expr_t)
  | Spawn of expr_t
  | Snd of expr_t
  | Seq of (expr_t * expr_t)
  | Send of (expr_t * expr_t)
  | Self
  | Right of expr_t
  | Ret of value_t
  | Receive
  | RecFunc of (string_t * string_t * expr_t)
  | Pair of (expr_t * expr_t)
  | Neg of expr_t
  | Mul of (expr_t * expr_t)
  | Match of (expr_t * expr_t * expr_t)
  | Let of (string_t * expr_t * expr_t)
  | Left of expr_t
  | Func of (string_t * expr_t)
  | Fst of expr_t
  | Div of (expr_t * expr_t)
  | Call of (expr_t * expr_t)
  | Add of (expr_t * expr_t)
  [@@deriving sexp]
  and either_t =
  | RightVal of value_t
  | LeftVal of value_t
  and bool_t =
  | True
  | False
  and act_env_t = (value_t queue_t) dict_t
  and env_t = string_t * (string_t * expr_t) queue_t * (value_t queue_t) dict_t * value_t dict_t
  and func_env_t = value_t dict_t
  and func_t = string_t * expr_t
  and inbox_t = value_t queue_t
  and pair_t = value_t * value_t
  and rec_func_t = string_t * string_t * expr_t
  and thread_pool_t = (string_t * expr_t) queue_t

  let make_int num = IntVal num

  let make_bool (b: bool) =
    match b with
    | true -> True
    | false -> False

  let make_name name = name

  let make_env () =
    let pid = "[root]" in
    let pool = Queue.of_list [] in
    let chenv = Map.empty (module String) in
    let fenv = Map.empty (module String) in
    (pid, pool, chenv, fenv)

end

(** All types, and the unspecified terms *)
module type UNSPEC = sig
  include TYPES
  module M: Common.Monads.MONAD

  val dict_drop: 'v dict_t * string_t -> ('v dict_t) M.t
  val dict_has_some: 'v dict_t * string_t -> bool_t M.t
  val dict_is_empty: 'v dict_t -> bool_t M.t
  val dict_new: unit -> ('v dict_t) M.t
  val dict_read: 'v dict_t * string_t -> 'v M.t
  val dict_write: 'v dict_t * string_t * 'v -> ('v dict_t) M.t
  val expr_throw_trace: env_t * expr_t -> (env_t * expr_t) M.t
  val int_add: int_t * int_t -> int_t M.t
  val int_div: int_t * int_t -> int_t M.t
  val int_mul: int_t * int_t -> int_t M.t
  val int_neg: int_t -> int_t M.t
  val int_sub: int_t * int_t -> int_t M.t
  val queue_dequeue: 'v queue_t -> ('v queue_t * 'v) M.t
  val queue_enqueue: 'v queue_t * 'v -> ('v queue_t) M.t
  val queue_is_empty: 'v queue_t -> bool_t M.t
  val queue_new: unit -> ('v queue_t) M.t
  val string_eq: string_t * string_t -> bool_t M.t
  val string_unique_id: unit -> string_t M.t
  val string_of_expr: expr_t -> string
  val expr_of_string: string -> expr_t
  val string_of_value: value_t -> string
  val value_of_string: string -> value_t
end

(** A default instantiation *)
module Unspec = struct
  open Base
  include Types
  module M = Common.Monads.Identity

  exception TraceFail of string
    
  let string_of_expr expr = sexp_of_expr_t expr |> Common.Utilities.string_of_sexp
  let expr_of_string strn = Core.Sexp.of_string strn |> expr_t_of_sexp
  let string_of_value value = sexp_of_value_t value |> Common.Utilities.string_of_sexp
  let value_of_string strn = Core.Sexp.of_string strn |> value_t_of_sexp

  let int_neg (num) =
    (-num)
    |> M.ret

  let int_add (lhs, rhs) = 
    (lhs + rhs)
    |> M.ret

  let int_sub (lhs, rhs) = 
    (lhs - rhs)
    |> M.ret

  let int_mul (lhs, rhs) = 
    (lhs * rhs)
    |> M.ret

  let int_div (lhs, rhs) = 
    (lhs / rhs)
    |> M.ret

  let string_eq (a, b) = 
    String.equal a b
    |> make_bool
    |> M.ret

  let next_id =
    ref 0

  let string_unique_id () =
    let id = ! next_id in
    next_id := id + 1 ;
    Printf.sprintf "[%d]" id
    |> M.ret

  let dict_new () =
    Map.empty (module String)
    |> M.ret

  let dict_drop (dict, key) =
    Map.remove dict key
    |> M.ret
    
  let dict_has_some (dict, key) =
    ( match Map.find dict key with
      | Some _ -> True
      | None   -> False
    ) |> M.ret

  let dict_is_empty dict =
    Map.is_empty dict
    |> make_bool
    |> M.ret

  let dict_read (dict, key) =
    match Map.find dict key with
      | Some v -> v |> M.ret
      | None   -> M.fail "Key not found"

  let dict_write (dict, key, value) =
    Map.set dict ~key:key ~data:value
    |> M.ret

  let queue_new () =
    Queue.of_list []
    |> M.ret

  let queue_is_empty queue =
    Queue.is_empty queue
    |> make_bool
    |> M.ret

  let queue_dequeue queue = 
    match Queue.dequeue queue with
    | Some v -> M.ret (queue, v)
    | None   -> M.fail "Queue is empty"

  let queue_enqueue (queue, value) = 
    Queue.enqueue queue value ;
    M.ret queue
    
  let expr_throw_trace (_, expr) = raise (TraceFail ("Could not match " ^ (string_of_expr expr)))
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: Common.Monads.MONAD

  type _ dict_t
  type int_t
  type _ queue_t
  type string_t

  type value_t =
  | UnitVal
  | RecFuncVal of rec_func_t
  | PidVal of string_t
  | PairVal of pair_t
  | IntVal of int_t
  | FuncVal of func_t
  | EitherVal of either_t
  and expr_t =
  | Var of string_t
  | Sub of (expr_t * expr_t)
  | Spawn of expr_t
  | Snd of expr_t
  | Seq of (expr_t * expr_t)
  | Send of (expr_t * expr_t)
  | Self
  | Right of expr_t
  | Ret of value_t
  | Receive
  | RecFunc of (string_t * string_t * expr_t)
  | Pair of (expr_t * expr_t)
  | Neg of expr_t
  | Mul of (expr_t * expr_t)
  | Match of (expr_t * expr_t * expr_t)
  | Let of (string_t * expr_t * expr_t)
  | Left of expr_t
  | Func of (string_t * expr_t)
  | Fst of expr_t
  | Div of (expr_t * expr_t)
  | Call of (expr_t * expr_t)
  | Add of (expr_t * expr_t)
  and either_t =
  | RightVal of value_t
  | LeftVal of value_t
  and bool_t =
  | True
  | False
  and act_env_t = (value_t queue_t) dict_t
  and env_t = string_t * (string_t * expr_t) queue_t * (value_t queue_t) dict_t * value_t dict_t
  and func_env_t = value_t dict_t
  and func_t = string_t * expr_t
  and inbox_t = value_t queue_t
  and pair_t = value_t * value_t
  and rec_func_t = string_t * string_t * expr_t
  and thread_pool_t = (string_t * expr_t) queue_t

  val actor_receive: env_t * string_t -> (env_t * value_t) M.t
  val actor_self: env_t -> string_t M.t
  val actor_send: env_t * string_t * value_t -> env_t M.t
  val actor_spawn: env_t * expr_t -> (env_t * string_t) M.t
  val bool_and: bool_t * bool_t -> bool_t M.t
  val bool_not: bool_t -> bool_t M.t
  val bool_or: bool_t * bool_t -> bool_t M.t
  val dict_drop: 'v dict_t * string_t -> ('v dict_t) M.t
  val dict_has_some: 'v dict_t * string_t -> bool_t M.t
  val dict_is_empty: 'v dict_t -> bool_t M.t
  val dict_new: unit -> ('v dict_t) M.t
  val dict_read: 'v dict_t * string_t -> 'v M.t
  val dict_write: 'v dict_t * string_t * 'v -> ('v dict_t) M.t
  val either_as_left: either_t -> value_t M.t
  val either_as_right: either_t -> value_t M.t
  val either_as_value: either_t -> value_t M.t
  val env_get_functions: env_t -> func_env_t M.t
  val env_get_inbox: env_t * string_t -> inbox_t M.t
  val env_get_pid: env_t -> string_t M.t
  val env_get_threads: env_t -> thread_pool_t M.t
  val env_read_func: env_t * string_t -> value_t M.t
  val env_set_functions: env_t * func_env_t -> env_t M.t
  val env_set_inbox: env_t * string_t * inbox_t -> env_t M.t
  val env_set_pid: env_t * string_t -> env_t M.t
  val env_set_threads: env_t * thread_pool_t -> env_t M.t
  val env_write_func: env_t * string_t * value_t -> env_t M.t
  val expr_reduce: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_add: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_call: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_div: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_fst: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_func: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_left: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_let: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_match: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_mul: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_neg: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_pair: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_rec_func: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_receive: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_ret: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_right: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_self: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_send: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_seq: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_snd: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_spawn: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_reduce_sub: env_t * expr_t -> (env_t * expr_t) M.t
  val expr_throw_trace: env_t * expr_t -> (env_t * expr_t) M.t
  val func_as_value: func_t -> value_t M.t
  val func_subst_in: string_t * value_t * expr_t -> expr_t M.t
  val int_add: int_t * int_t -> int_t M.t
  val int_as_value: int_t -> value_t M.t
  val int_div: int_t * int_t -> int_t M.t
  val int_mul: int_t * int_t -> int_t M.t
  val int_neg: int_t -> int_t M.t
  val int_sub: int_t * int_t -> int_t M.t
  val pair_as_value: pair_t -> value_t M.t
  val pid_as_value: string_t -> value_t M.t
  val queue_dequeue: 'v queue_t -> ('v queue_t * 'v) M.t
  val queue_enqueue: 'v queue_t * 'v -> ('v queue_t) M.t
  val queue_is_empty: 'v queue_t -> bool_t M.t
  val queue_new: unit -> ('v queue_t) M.t
  val rec_func_as_value: rec_func_t -> value_t M.t
  val string_eq: string_t * string_t -> bool_t M.t
  val string_unique_id: unit -> string_t M.t
  val unint_as_value: unit -> value_t M.t
  val value_as_either: value_t -> either_t M.t
  val value_as_func: value_t -> func_t M.t
  val value_as_int: value_t -> int_t M.t
  val value_as_pair: value_t -> pair_t M.t
  val value_as_pid: value_t -> string_t M.t
  val value_as_rec_func: value_t -> rec_func_t M.t
  val value_as_unit: value_t -> value_t M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F
  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec actor_receive =
    function (env, pid) ->
    let* inbox = apply1 env_get_inbox (env, pid) in
    let* (inbox', msg) = apply1 queue_dequeue inbox in
    let* env' = apply1 env_set_inbox (env, pid, inbox') in
    M.ret (env', msg)
  and actor_self env = apply1 env_get_pid env
  and actor_send =
    function (env, pid, value) ->
    let* inbox = apply1 env_get_inbox (env, pid) in
    let* inbox' = apply1 queue_enqueue (inbox, value) in
    let* env' = apply1 env_set_inbox (env, pid, inbox') in
    M.ret env'
  and actor_spawn =
    function (env, expr) ->
    let* pid = apply1 string_unique_id () in
    let* tpool = apply1 env_get_threads env in
    let* tpool' = apply1 queue_enqueue (tpool, (pid, expr)) in
    let* env' = apply1 env_set_threads (env, tpool') in
    let* inbox = apply1 queue_new () in
    let* env'' = apply1 env_set_inbox (env', pid, inbox) in
    M.ret (env'', pid)
  and bool_and =
    function (a, b) ->
    M.branch [
      (function () ->
        begin match a with
        | False -> M.ret False
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match b with
        | False -> M.ret False
        | _ -> M.fail ""
        end) ;
      (function () ->
        M.ret True)]
  and bool_not b =
    M.branch [
      (function () ->
        begin match b with
        | True -> M.ret False
        | _ -> M.fail ""
        end) ;
      (function () ->
        M.ret True)]
  and bool_or =
    function (a, b) ->
    M.branch [
      (function () ->
        begin match a with
        | True -> M.ret True
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match b with
        | True -> M.ret True
        | _ -> M.fail ""
        end) ;
      (function () ->
        M.ret False)]
  and either_as_left e =
    begin match e with
    | LeftVal v -> M.ret v
    | _ -> M.fail ""
    end
  and either_as_right e =
    begin match e with
    | RightVal v -> M.ret v
    | _ -> M.fail ""
    end
  and either_as_value e = M.ret (EitherVal e)
  and env_get_functions env =
    let (_, _, _, fn) = env in
    M.ret fn
  and env_get_inbox =
    function (env, pid) ->
    let (_, _, boxes, _) = env in
    let* inbox = apply1 dict_read (boxes, pid) in
    M.ret inbox
  and env_get_pid env =
    let (pid, _, _, _) = env in
    M.ret pid
  and env_get_threads env =
    let (_, th, _, _) = env in
    M.ret th
  and env_read_func =
    function (env, name) ->
    let* funs = apply1 env_get_functions env in
    let* f = apply1 dict_read (funs, name) in
    M.ret f
  and env_set_functions =
    function (env, fn') ->
    let (pid, th, ch, _) = env in
    M.ret (pid, th, ch, fn')
  and env_set_inbox =
    function (env, pid, inbox) ->
    let (self, th, boxes, fn) = env in
    let* boxes' = apply1 dict_write (boxes, pid, inbox) in
    M.ret (self, th, boxes', fn)
  and env_set_pid =
    function (env, pid') ->
    let (_, th, ch, fn) = env in
    M.ret (pid', th, ch, fn)
  and env_set_threads =
    function (env, th') ->
    let (pid, _, ch, fn) = env in
    M.ret (pid, th', ch, fn)
  and env_write_func =
    function (env, name, func) ->
    let* funcs = apply1 env_get_functions env in
    let* funcs' = apply1 dict_write (funcs, name, func) in
    let* env' = apply1 env_set_functions (env, funcs') in
    M.ret env'
  and expr_reduce =
    function (env, expr) ->
    M.branch [
      (function () ->
        apply1 expr_reduce_ret (env, expr)) ;
      (function () ->
        apply1 expr_reduce_seq (env, expr)) ;
      (function () ->
        apply1 expr_reduce_func (env, expr)) ;
      (function () ->
        apply1 expr_reduce_call (env, expr)) ;
      (function () ->
        apply1 expr_reduce_let (env, expr)) ;
      (function () ->
        apply1 expr_reduce_neg (env, expr)) ;
      (function () ->
        apply1 expr_reduce_add (env, expr)) ;
      (function () ->
        apply1 expr_reduce_sub (env, expr)) ;
      (function () ->
        apply1 expr_reduce_mul (env, expr)) ;
      (function () ->
        apply1 expr_reduce_div (env, expr)) ;
      (function () ->
        apply1 expr_reduce_pair (env, expr)) ;
      (function () ->
        apply1 expr_reduce_fst (env, expr)) ;
      (function () ->
        apply1 expr_reduce_snd (env, expr)) ;
      (function () ->
        apply1 expr_reduce_left (env, expr)) ;
      (function () ->
        apply1 expr_reduce_right (env, expr)) ;
      (function () ->
        apply1 expr_reduce_match (env, expr)) ;
      (function () ->
        apply1 expr_reduce_self (env, expr)) ;
      (function () ->
        apply1 expr_reduce_receive (env, expr)) ;
      (function () ->
        apply1 expr_reduce_spawn (env, expr)) ;
      (function () ->
        apply1 expr_reduce_send (env, expr)) ;
      (function () ->
        apply1 expr_throw_trace (env, expr))]
  and expr_reduce_add =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Add (Ret lhs, Ret rhs) ->
            let* i = apply1 value_as_int lhs in
            let* j = apply1 value_as_int rhs in
            let* res = apply1 int_add (i, j) in
            let* res' = apply1 int_as_value res in
            M.ret (env, Ret res')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Add (Ret lhs, rhs) ->
            let* (env', rhs') = apply1 expr_reduce (env, rhs) in
            M.ret (env', Add (Ret lhs, rhs'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Add (lhs, rhs) ->
            let* (env', lhs') = apply1 expr_reduce (env, lhs) in
            M.ret (env', Add (lhs', rhs))
        | _ -> M.fail ""
        end)]
  and expr_reduce_call =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Call (Ret func, Ret arg_val) ->
            M.branch [
              (function () ->
                let* (arg_name, body) = apply1 value_as_func func in
                let* body' = apply1 func_subst_in (arg_name, arg_val, body) in
                M.ret (env, body')) ;
              (function () ->
                let* (func_name, arg_name, body) = apply1 value_as_rec_func func in
                let* body' = apply1 func_subst_in (func_name, func, body) in
                let* body'' = apply1 func_subst_in (arg_name, arg_val, body') in
                M.ret (env, body''))]
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Call (Ret func, arg) ->
            let* (env', arg') = apply1 expr_reduce (env, arg) in
            M.ret (env', Call (Ret func, arg'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Call (func, arg) ->
            let* (env', func') = apply1 expr_reduce (env, func) in
            M.ret (env', Call (func', arg))
        | _ -> M.fail ""
        end)]
  and expr_reduce_div =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Div (Ret lhs, Ret rhs) ->
            let* i = apply1 value_as_int lhs in
            let* j = apply1 value_as_int rhs in
            let* res = apply1 int_div (i, j) in
            let* res' = apply1 int_as_value res in
            M.ret (env, Ret res')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Div (Ret lhs, rhs) ->
            let* (env', rhs') = apply1 expr_reduce (env, rhs) in
            M.ret (env', Div (Ret lhs, rhs'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Div (lhs, rhs) ->
            let* (env', lhs') = apply1 expr_reduce (env, lhs) in
            M.ret (env', Div (lhs', rhs))
        | _ -> M.fail ""
        end)]
  and expr_reduce_fst =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Fst Ret pair ->
            let* (fst, _) = apply1 value_as_pair pair in
            M.ret (env, Ret fst)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Fst pair ->
            let* (env', pair') = apply1 expr_reduce (env, pair) in
            M.ret (env', Fst pair')
        | _ -> M.fail ""
        end)]
  and expr_reduce_func =
    function (env, expr) ->
    begin match expr with
    | Func (name, expr) ->
        let func = FuncVal (name, expr) in
        M.ret (env, Ret func)
    | _ -> M.fail ""
    end
  and expr_reduce_left =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Left Ret load ->
            let either = EitherVal (LeftVal load) in
            M.ret (env, Ret either)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Left load ->
            let* (env', load') = apply1 expr_reduce (env, load) in
            M.ret (env', Left load')
        | _ -> M.fail ""
        end)]
  and expr_reduce_let =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Let (name, Ret value, body) ->
            let* res = apply1 func_subst_in (name, value, body) in
            M.ret (env, res)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Let (name, value, body) ->
            let* (env', value') = apply1 expr_reduce (env, value) in
            M.ret (env', Let (name, value', body))
        | _ -> M.fail ""
        end)]
  and expr_reduce_match =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Match (Ret either, on_left, on_right) ->
            let* either = apply1 value_as_either either in
            M.branch [
              (function () ->
                let* value = apply1 either_as_left either in
                M.ret (env, Call (on_left, Ret value))) ;
              (function () ->
                let* value = apply1 either_as_right either in
                M.ret (env, Call (on_right, Ret value)))]
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Match (either, on_left, on_right) ->
            let* (env', either') = apply1 expr_reduce (env, either) in
            M.ret (env', Match (either', on_left, on_right))
        | _ -> M.fail ""
        end)]
  and expr_reduce_mul =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Mul (Ret lhs, Ret rhs) ->
            let* i = apply1 value_as_int lhs in
            let* j = apply1 value_as_int rhs in
            let* res = apply1 int_mul (i, j) in
            let* res' = apply1 int_as_value res in
            M.ret (env, Ret res')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Mul (Ret lhs, rhs) ->
            let* (env', rhs') = apply1 expr_reduce (env, rhs) in
            M.ret (env', Mul (Ret lhs, rhs'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Mul (lhs, rhs) ->
            let* (env', lhs') = apply1 expr_reduce (env, lhs) in
            M.ret (env', Mul (lhs', rhs))
        | _ -> M.fail ""
        end)]
  and expr_reduce_neg =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Neg Ret value ->
            let* int = apply1 value_as_int value in
            let* int' = apply1 int_neg int in
            let* v = apply1 int_as_value int' in
            M.ret (env, Ret v)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Neg expr ->
            let* (env', expr') = apply1 expr_reduce (env, expr) in
            M.ret (env', Neg expr')
        | _ -> M.fail ""
        end)]
  and expr_reduce_pair =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Pair (Ret fst, Ret snd) ->
            let pair = PairVal (fst, snd) in
            M.ret (env, Ret pair)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Pair (Ret fst, snd) ->
            let* (env', snd') = apply1 expr_reduce (env, snd) in
            M.ret (env', Pair (Ret fst, snd'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Pair (fst, snd) ->
            let* (env', fst') = apply1 expr_reduce (env, fst) in
            M.ret (env', Pair (fst', snd))
        | _ -> M.fail ""
        end)]
  and expr_reduce_rec_func =
    function (env, expr) ->
    begin match expr with
    | RecFunc (func_name, arg_name, expr) ->
        let func = RecFuncVal (func_name, arg_name, expr) in
        M.ret (env, Ret func)
    | _ -> M.fail ""
    end
  and expr_reduce_receive =
    function (env, expr) ->
    begin match expr with
    | Receive ->
        let* pid = apply1 actor_self env in
        let* (env', msg) = apply1 actor_receive (env, pid) in
        M.ret (env', Ret msg)
    | _ -> M.fail ""
    end
  and expr_reduce_ret =
    function (env, expr) ->
    begin match expr with
    | Ret value -> M.ret (env, Ret value)
    | _ -> M.fail ""
    end
  and expr_reduce_right =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Right Ret load ->
            let either = EitherVal (RightVal load) in
            M.ret (env, Ret either)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Right load ->
            let* (env', load') = apply1 expr_reduce (env, load) in
            M.ret (env', Right load')
        | _ -> M.fail ""
        end)]
  and expr_reduce_self =
    function (env, expr) ->
    begin match expr with
    | Self ->
        let* pid = apply1 actor_self env in
        let pid' = PidVal pid in
        M.ret (env, Ret pid')
    | _ -> M.fail ""
    end
  and expr_reduce_send =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Send (Ret value, Ret PidVal pid) ->
            let* env' = apply1 actor_send (env, pid, value) in
            M.ret (env', Ret UnitVal)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Send (Ret value, target) ->
            let* (env', target') = apply1 expr_reduce (env, target) in
            M.ret (env', Send (Ret value, target'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Send (value, target) ->
            let* (env', value') = apply1 expr_reduce (env, value) in
            M.ret (env', Send (value', target))
        | _ -> M.fail ""
        end)]
  and expr_reduce_seq =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Seq (Ret _, rest) -> M.ret (env, rest)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Seq (this, rest) ->
            let* (env', this') = apply1 expr_reduce (env, this) in
            M.ret (env', Seq (this', rest))
        | _ -> M.fail ""
        end)]
  and expr_reduce_snd =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Snd Ret pair ->
            let* (_, snd) = apply1 value_as_pair pair in
            M.ret (env, Ret snd)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Snd pair ->
            let* (env', pair') = apply1 expr_reduce (env, pair) in
            M.ret (env', Snd pair')
        | _ -> M.fail ""
        end)]
  and expr_reduce_spawn =
    function (env, expr) ->
    begin match expr with
    | Spawn expr ->
        let* (env', pid) = apply1 actor_spawn (env, expr) in
        let actor = PidVal pid in
        M.ret (env', Ret actor)
    | _ -> M.fail ""
    end
  and expr_reduce_sub =
    function (env, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Sub (Ret lhs, Ret rhs) ->
            let* i = apply1 value_as_int lhs in
            let* j = apply1 value_as_int rhs in
            let* res = apply1 int_sub (i, j) in
            let* res' = apply1 int_as_value res in
            M.ret (env, Ret res')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Sub (Ret lhs, rhs) ->
            let* (env', rhs') = apply1 expr_reduce (env, rhs) in
            M.ret (env', Sub (Ret lhs, rhs'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Sub (lhs, rhs) ->
            let* (env', lhs') = apply1 expr_reduce (env, lhs) in
            M.ret (env', Sub (lhs', rhs))
        | _ -> M.fail ""
        end)]
  and func_as_value f = M.ret (FuncVal f)
  and func_subst_in =
    function (par, arg, expr) ->
    M.branch [
      (function () ->
        begin match expr with
        | Ret _ -> M.ret expr
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Var name ->
            M.branch [
              (function () ->
                let* _tmp = apply1 string_eq (name, par) in
                begin match _tmp with
                | True -> M.ret (Ret arg)
                | _ -> M.fail ""
                end) ;
              (function () ->
                M.ret expr)]
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Seq (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Seq (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Func (other1, expr1) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Func (other1, expr1'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | RecFunc (other1, other2, expr1) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (RecFunc (other1, other2, expr1'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Call (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Call (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Let (other1, expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Let (other1, expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Neg expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Neg expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Add (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Add (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Sub (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Sub (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Mul (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Mul (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Div (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Div (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Pair (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Pair (expr1', expr2'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Fst expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Fst expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Snd expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Snd expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Left expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Left expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Right expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Right expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Match (expr1, expr2, expr3) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            let* expr3' = apply1 func_subst_in (par, arg, expr3) in
            M.ret (Match (expr1', expr2', expr3'))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Self -> M.ret Self
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Receive -> M.ret Receive
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Spawn expr1 ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            M.ret (Spawn expr1')
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match expr with
        | Send (expr1, expr2) ->
            let* expr1' = apply1 func_subst_in (par, arg, expr1) in
            let* expr2' = apply1 func_subst_in (par, arg, expr2) in
            M.ret (Send (expr1', expr2'))
        | _ -> M.fail ""
        end)]
  and int_as_value i = M.ret (IntVal i)
  and pair_as_value p = M.ret (PairVal p)
  and pid_as_value p = M.ret (PidVal p)
  and rec_func_as_value r = M.ret (RecFuncVal r)
  and unint_as_value =
    function _ ->
    M.ret UnitVal
  and value_as_either v =
    begin match v with
    | EitherVal e -> M.ret e
    | _ -> M.fail ""
    end
  and value_as_func v =
    begin match v with
    | FuncVal f -> M.ret f
    | _ -> M.fail ""
    end
  and value_as_int v =
    begin match v with
    | IntVal i -> M.ret i
    | _ -> M.fail ""
    end
  and value_as_pair v =
    begin match v with
    | PairVal p -> M.ret p
    | _ -> M.fail ""
    end
  and value_as_pid v =
    begin match v with
    | PidVal p -> M.ret p
    | _ -> M.fail ""
    end
  and value_as_rec_func v =
    begin match v with
    | RecFuncVal rf -> M.ret rf
    | _ -> M.fail ""
    end
  and value_as_unit v =
    begin match v with
    | UnitVal -> M.ret UnitVal
    | _ -> M.fail ""
    end
end

module Interpreter = MakeInterpreter(Unspec)
