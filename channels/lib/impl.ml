module type TYPES = sig
  type int_t
  and string_t
  and _ queue_t
  and _ dict_t

  and chan_buff_t = value_t queue_t
  and chan_env_t = chan_buff_t dict_t
  and chan_id_t = string_t
  and env_t = thread_pool_t * chan_env_t * func_env_t
  and func_env_t = value_t dict_t
  and func_t = string_t * expr_t * env_t
  and name_t = string_t
  and pair_t = value_t * value_t
  and rec_func_t = string_t * string_t * expr_t * env_t
  and thread_pool_t = expr_t queue_t
  
  and value_t =
  | UnitVal
  | RecFuncVal of rec_func_t
  | PairVal of pair_t
  | IntVal of int_t
  | FuncVal of func_t
  | EitherVal of either_t
  | ChanIdVal of chan_id_t

  and expr_t =
  | Var of name_t
  | Take of chan_id_t
  | Sub of (expr_t * expr_t)
  | Snd of expr_t
  | Seq of (expr_t * expr_t)
  | Right of expr_t
  | Ret of value_t
  | RecFunc of (name_t * name_t * expr_t)
  | Pair of (expr_t * expr_t)
  | NewCh
  | Neg of expr_t
  | Mul of (expr_t * expr_t)
  | Match of (expr_t * expr_t * expr_t)
  | Let of (name_t * expr_t * expr_t)
  | Left of expr_t
  | Give of (chan_id_t * expr_t)
  | Func of (name_t * expr_t)
  | Fst of expr_t
  | Fork of expr_t
  | Div of (expr_t * expr_t)
  | Call of (expr_t * expr_t)
  | Add of (expr_t * expr_t)

  and either_t =
  | RightVal of value_t
  | LeftVal of value_t
end

module type SPEC = sig
  include TYPES
  module M : Monads.MONAD

  val env_new: unit -> expr_t queue_t * chan_buff_t dict_t * value_t dict_t

  val string_of_expr: expr_t -> String.t
  val string_of_value: value_t -> String.t

  val int_add: int_t * int_t -> value_t M.t
  val int_div: int_t * int_t -> value_t M.t
  val int_mul: int_t * int_t -> value_t M.t
  val int_neg: int_t -> value_t M.t
  val int_sub: int_t * int_t -> value_t M.t

  val string_eq: string_t * string_t -> unit M.t
  val string_unique_id: unit -> string_t M.t

  val queue_dequeue: 'v queue_t -> ('v queue_t * 'v) M.t
  val queue_enqueue: 'v queue_t * 'v -> ('v queue_t) M.t
  val queue_is_empty: 'v queue_t -> ('v queue_t) M.t
  val queue_is_not_empty: 'v queue_t -> ('v queue_t) M.t
  val queue_new: unit -> ('v queue_t) M.t

  val dict_drop: 'v dict_t * string_t -> ('v dict_t) M.t
  val dict_has_no: 'v dict_t * string_t -> ('v dict_t) M.t
  val dict_has_some: 'v dict_t * string_t -> ('v dict_t) M.t
  val dict_is_empty: 'v dict_t -> ('v dict_t) M.t
  val dict_is_empty_not: 'v dict_t -> ('v dict_t) M.t
  val dict_new: unit -> ('v dict_t) M.t
  val dict_read: 'v dict_t * string_t -> 'v M.t
  val dict_write: 'v dict_t * string_t * 'v -> ('v dict_t) M.t
end

module Types = struct
  open Base
  
  type int_t = Int.t
  and string_t = String.t
  and 'a queue_t = 'a Queue.t
  and 'v dict_t = 'v Map.M(String).t
  and chan_buff_t = value_t queue_t
  and chan_env_t = chan_buff_t dict_t
  and chan_id_t = string_t
  and env_t = thread_pool_t * chan_env_t * value_t dict_t
  and func_env_t = value_t dict_t
  and func_t = string_t * expr_t * env_t
  and name_t = string_t
  and pair_t = value_t * value_t
  and rec_func_t = string_t * string_t * expr_t * env_t
  and thread_pool_t = expr_t queue_t
  
  and value_t =
  | UnitVal
  | RecFuncVal of rec_func_t
  | PairVal of pair_t
  | IntVal of int_t
  | FuncVal of func_t
  | EitherVal of either_t
  | ChanIdVal of chan_id_t

  and expr_t =
  | Var of name_t
  | Take of chan_id_t
  | Sub of (expr_t * expr_t)
  | Snd of expr_t
  | Seq of (expr_t * expr_t)
  | Right of expr_t
  | Ret of value_t
  | RecFunc of (name_t * name_t * expr_t)
  | Pair of (expr_t * expr_t)
  | NewCh
  | Neg of expr_t
  | Mul of (expr_t * expr_t)
  | Match of (expr_t * expr_t * expr_t)
  | Let of (name_t * expr_t * expr_t)
  | Left of expr_t
  | Give of (chan_id_t * expr_t)
  | Func of (name_t * expr_t)
  | Fst of expr_t
  | Fork of expr_t
  | Div of (expr_t * expr_t)
  | Call of (expr_t * expr_t)
  | Add of (expr_t * expr_t)

  and either_t =
  | RightVal of value_t
  | LeftVal of value_t

end

module Spec = struct
  include Types
  module M = Monads.Identity

  exception BranchFail of string

  open Base

  let rec string_of_expr expr =
    ( match expr with
    | Ret value ->                  string_of_value value
    | Var name ->                   name
    | Func (name, body) ->          Printf.sprintf "(%s) -> (%s)" name (string_of_expr body)
    | RecFunc (func, arg, body) ->  Printf.sprintf "%s:(%s) -> (%s)" func arg (string_of_expr body)
    | Let (name, value, body) ->    Printf.sprintf "let %s = %s in\n%s" name (string_of_expr value) (string_of_expr body)
    | Seq (left, right) ->          Printf.sprintf "%s ;\n%s" (string_of_expr left) (string_of_expr right)
    | Neg (num) ->                  Printf.sprintf "-%s" (string_of_expr num)
    | Add (left, right) ->          Printf.sprintf "%s + %s" (string_of_expr left) (string_of_expr right)
    | Sub (left, right) ->          Printf.sprintf "%s - %s" (string_of_expr left) (string_of_expr right)
    | Mul (left, right) ->          Printf.sprintf "%s * %s" (string_of_expr left) (string_of_expr right)
    | Div (left, right) ->          Printf.sprintf "%s / %s" (string_of_expr left) (string_of_expr right)
    | Left either ->                Printf.sprintf "left (%s)" (string_of_expr either)
    | Right either ->               Printf.sprintf "right (%s)" (string_of_expr either)
    | Match (guard, left, right) -> Printf.sprintf "match (%s) with\n| Left %s\n|Right %s" (string_of_expr guard) (string_of_expr left) (string_of_expr right)
    | Pair (left, right) ->         Printf.sprintf "(%s, %s)" (string_of_expr left) (string_of_expr right)
    | Fst pair ->                   Printf.sprintf "fst (%s)" (string_of_expr pair)
    | Snd pair ->                   Printf.sprintf "snd (%s)" (string_of_expr pair)
    | NewCh ->                      "NewCh"
    | Fork expr ->                  Printf.sprintf "fork(%s)" (string_of_expr expr)
    | Call (func, arg) ->           Printf.sprintf "(%s) (%s)" (string_of_expr func) (string_of_expr arg)
    | Give (chan, value) ->         Printf.sprintf "%s! %s" chan (string_of_expr value)
    | Take chan ->                  Printf.sprintf "%s?" chan
    )
    
  and string_of_value value = 
    ( match value with
    | UnitVal                         -> "()"
    | RecFuncVal (func, arg, body, _) -> Printf.sprintf "<fun %s:(%s) -> (%s)>" func arg (string_of_expr body)
    | PairVal (l, r)                  -> Printf.sprintf "<(%s, %s)>" (string_of_value l) (string_of_value r)
    | IntVal (i)                      -> Printf.sprintf "<%d>" i
    | FuncVal (arg, body, _)          -> Printf.sprintf "<fun (%s) -> (%s)>" arg (string_of_expr body)
    | EitherVal (LeftVal v)           -> Printf.sprintf "<Left(%s)>" (string_of_value v)
    | EitherVal (RightVal v)          -> Printf.sprintf "<Right(%s)>" (string_of_value v)
    | ChanIdVal (chan)                -> Printf.sprintf "<%s>" chan
    )

  let int_neg i = IntVal (-i) |> M.ret
  let int_add (i, j) = IntVal (i + j) |> M.ret
  let int_sub (i, j) = IntVal (i - j) |> M.ret
  let int_mul (i, j) = IntVal (i * j) |> M.ret
  let int_div (i, j) = IntVal (i / j) |> M.ret


  let next_id = ref 0

  let string_eq (a, b) =
    ( match (String.equal a b) with
      | true -> ()
      | false -> raise (BranchFail "Equality failed")
    )
    |> M.ret

  let string_unique_id () =
    let id = ! next_id in
    next_id := id + 1 ;
    Printf.sprintf "<%d>" id
    |> M.ret


  let queue_new () =
    Queue.of_list []
    |> M.ret

  let queue_is_empty queue =
    match (Queue.is_empty queue) with
    | true -> queue |> M.ret
    | false -> raise (BranchFail "Queue is not empty")


  let queue_is_not_empty queue =
    match not (Queue.is_empty queue) with
    | true -> queue |> M.ret
    | false -> raise (BranchFail "Queue is empty")


  let queue_enqueue (queue, value) =
    Queue.enqueue queue value ;
    queue |> M.ret

  let queue_dequeue queue = 
    ( queue,
      ( match Queue.dequeue queue with
        | Some value -> value
        | None       -> raise (BranchFail "Queue is empty")
      )
    )
    |> M.ret
  

  let dict_new () =
    Map.empty (module String)
    |> M.ret

  let dict_is_empty dict =
    match (Map.is_empty dict) with
    | true -> dict |> M.ret
    | false -> raise (BranchFail "Dict is not empty")

  let dict_is_empty_not dict =
    match (not (Map.is_empty dict)) with
    | true -> dict |> M.ret
    | false -> raise (BranchFail "Dict is empty")

  let dict_has_some (dict, key) =
    match (Map.find dict key) with
    | Some _ -> dict |> M.ret
    | None   -> raise (BranchFail "Key not found")

  let dict_has_no ((dict, key): 'v dict_t * string_t) =
    match (Map.find dict key) with
    | None   -> dict |> M.ret
    | Some _ -> raise (BranchFail "Key exists")

  let dict_write (dict, key, value) =
    Map.set dict ~key:key ~data:value
    |> M.ret

  let dict_read (dict, key) = 
    ( match Map.find dict key with
    | Some value -> value
    | None       -> raise (BranchFail "Key not found")
    )
    |> M.ret

  let dict_drop (dict, key) =
    Map.remove dict key
    |> M.ret
    
  let env_new (): expr_t queue_t * chan_buff_t dict_t * value_t dict_t =
    let pool = queue_new () |> M.extract in
    let chenv = dict_new () |> M.extract in
    let fenv = dict_new () |> M.extract in
    (pool, chenv, fenv)
end
