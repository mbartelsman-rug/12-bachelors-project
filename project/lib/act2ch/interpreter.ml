(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: Common.Monads.MONAD

  type int_t
  and string_t
  and lit_t =
  | UnitVal
  | IntVal of int_t
  and expr_ch_t =
  | ChVar of name_t
  | ChTake of expr_ch_t
  | ChSub of (expr_ch_t * expr_ch_t)
  | ChSnd of expr_ch_t
  | ChSeq of (expr_ch_t * expr_ch_t)
  | ChRight of expr_ch_t
  | ChRet of lit_t
  | ChRecFunc of (name_t * name_t * expr_ch_t)
  | ChPair of (expr_ch_t * expr_ch_t)
  | ChNewCh
  | ChNeg of expr_ch_t
  | ChMul of (expr_ch_t * expr_ch_t)
  | ChMatch of (expr_ch_t * expr_ch_t * expr_ch_t)
  | ChLet of (name_t * expr_ch_t * expr_ch_t)
  | ChLeft of expr_ch_t
  | ChGive of (expr_ch_t * expr_ch_t)
  | ChFunc of (name_t * expr_ch_t)
  | ChFst of expr_ch_t
  | ChFork of expr_ch_t
  | ChDiv of (expr_ch_t * expr_ch_t)
  | ChCall of (expr_ch_t * expr_ch_t)
  | ChAdd of (expr_ch_t * expr_ch_t)
  and expr_act_t =
  | ActVar of name_t
  | ActSub of (expr_act_t * expr_act_t)
  | ActSpawn of expr_act_t
  | ActSnd of expr_act_t
  | ActSeq of (expr_act_t * expr_act_t)
  | ActSend of (expr_act_t * expr_act_t)
  | ActSelf
  | ActRight of expr_act_t
  | ActRet of lit_t
  | ActReceive
  | ActRecFunc of (name_t * name_t * expr_act_t)
  | ActPair of (expr_act_t * expr_act_t)
  | ActNeg of expr_act_t
  | ActMul of (expr_act_t * expr_act_t)
  | ActMatch of (expr_act_t * expr_act_t * expr_act_t)
  | ActLet of (name_t * expr_act_t * expr_act_t)
  | ActLeft of expr_act_t
  | ActFunc of (name_t * expr_act_t)
  | ActFst of expr_act_t
  | ActDiv of (expr_act_t * expr_act_t)
  | ActCall of (expr_act_t * expr_act_t)
  | ActAdd of (expr_act_t * expr_act_t)
  and chan_t = string_t
  and name_t = string_t

  val string_unique_id: unit -> string_t M.t
  val make_name: string -> string_t
  val make_int: int -> lit_t
  val string_of_ch_expr: expr_ch_t -> string
  val string_of_value: lit_t -> string
end

(** A default instantiation *)
module Unspec = struct
  open Base
  exception NotImplemented of string
  include T
  module M = Common.Monads.Identity

  type int_t = Int.t
  and string_t = String.t
  and lit_t =
  | UnitVal
  | IntVal of int_t
  and expr_ch_t =
  | ChVar of name_t
  | ChTake of expr_ch_t
  | ChSub of (expr_ch_t * expr_ch_t)
  | ChSnd of expr_ch_t
  | ChSeq of (expr_ch_t * expr_ch_t)
  | ChRight of expr_ch_t
  | ChRet of lit_t
  | ChRecFunc of (name_t * name_t * expr_ch_t)
  | ChPair of (expr_ch_t * expr_ch_t)
  | ChNewCh
  | ChNeg of expr_ch_t
  | ChMul of (expr_ch_t * expr_ch_t)
  | ChMatch of (expr_ch_t * expr_ch_t * expr_ch_t)
  | ChLet of (name_t * expr_ch_t * expr_ch_t)
  | ChLeft of expr_ch_t
  | ChGive of (expr_ch_t * expr_ch_t)
  | ChFunc of (name_t * expr_ch_t)
  | ChFst of expr_ch_t
  | ChFork of expr_ch_t
  | ChDiv of (expr_ch_t * expr_ch_t)
  | ChCall of (expr_ch_t * expr_ch_t)
  | ChAdd of (expr_ch_t * expr_ch_t)
  and expr_act_t =
  | ActVar of name_t
  | ActSub of (expr_act_t * expr_act_t)
  | ActSpawn of expr_act_t
  | ActSnd of expr_act_t
  | ActSeq of (expr_act_t * expr_act_t)
  | ActSend of (expr_act_t * expr_act_t)
  | ActSelf
  | ActRight of expr_act_t
  | ActRet of lit_t
  | ActReceive
  | ActRecFunc of (name_t * name_t * expr_act_t)
  | ActPair of (expr_act_t * expr_act_t)
  | ActNeg of expr_act_t
  | ActMul of (expr_act_t * expr_act_t)
  | ActMatch of (expr_act_t * expr_act_t * expr_act_t)
  | ActLet of (name_t * expr_act_t * expr_act_t)
  | ActLeft of expr_act_t
  | ActFunc of (name_t * expr_act_t)
  | ActFst of expr_act_t
  | ActDiv of (expr_act_t * expr_act_t)
  | ActCall of (expr_act_t * expr_act_t)
  | ActAdd of (expr_act_t * expr_act_t)
  and chan_t = string_t
  and name_t = string_t

  let next_id =
    ref 0

  let string_unique_id () =
    let id = ! next_id in
    next_id := id + 1 ;
    Printf.sprintf "[%d]" id
    |> M.ret

  let make_name s = s

  let make_int i = IntVal i
  
  let rec string_of_ch_expr expr =
    ( match expr with
    | ChRet value ->                  Printf.sprintf "Ret(%s)" (string_of_value value)
    | ChVar name ->                   Printf.sprintf "Var(\"%s\")" name
    | ChFunc (name, body) ->          Printf.sprintf "Func(\"%s\",%s)" name (string_of_ch_expr body)
    | ChRecFunc (func, arg, body) ->  Printf.sprintf "RecFunc(\"%s\",\"%s\",%s)" func arg (string_of_ch_expr body)
    | ChLet (name, value, body) ->    Printf.sprintf "Let(\"%s\",%s,%s)" name (string_of_ch_expr value) (string_of_ch_expr body)
    | ChSeq (left, right) ->          Printf.sprintf "Seq(%s,%s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChNeg (num) ->                  Printf.sprintf "Neg(%s)" (string_of_ch_expr num)
    | ChAdd (left, right) ->          Printf.sprintf "Add(%s,%s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChSub (left, right) ->          Printf.sprintf "Sub(%s,%s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChMul (left, right) ->          Printf.sprintf "Mul(%s,%s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChDiv (left, right) ->          Printf.sprintf "Div(%s,%s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChLeft either ->                Printf.sprintf "Left(%s)" (string_of_ch_expr either)
    | ChRight either ->               Printf.sprintf "Right(%s)" (string_of_ch_expr either)
    | ChMatch (guard, left, right) -> Printf.sprintf "Match(%s,%s,%s)" (string_of_ch_expr guard) (string_of_ch_expr left) (string_of_ch_expr right)
    | ChPair (left, right) ->         Printf.sprintf "Pain(%s, %s)" (string_of_ch_expr left) (string_of_ch_expr right)
    | ChFst pair ->                   Printf.sprintf "Fst(%s)" (string_of_ch_expr pair)
    | ChSnd pair ->                   Printf.sprintf "Snd(%s)" (string_of_ch_expr pair)
    | ChCall (func, arg) ->           Printf.sprintf "Call(%s,%s)" (string_of_ch_expr func) (string_of_ch_expr arg)
    | ChTake (chan) ->                Printf.sprintf "Take(%s)" (string_of_ch_expr chan)
    | ChNewCh ->                                     "NewCh"
    | ChGive (target, value) ->       Printf.sprintf "Give(%s,%s)" (string_of_ch_expr target) (string_of_ch_expr value)
    | ChFork expr ->                  Printf.sprintf "Fork(%s)" (string_of_ch_expr expr)
    )
    
  and string_of_value value = 
    ( match value with
    | UnitVal                         ->                "()"
    | IntVal (i)                      -> Printf.sprintf "%d" i
    )
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  include UNSPEC

  val translate: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_add: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_call: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_div: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_fst: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_func: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_left: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_let: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_match: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_mul: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_neg: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_pair: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_rec_func: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_receive: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_ret: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_right: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_self: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_send: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_seq: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_snd: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_spawn: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_sub: expr_act_t * chan_t -> expr_ch_t M.t
  val translate_var: expr_act_t * chan_t -> expr_ch_t M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec translate =
    function (expr, ch) ->
    M.branch [
      (function () ->
        apply1 translate_ret (expr, ch)) ;
      (function () ->
        apply1 translate_var (expr, ch)) ;
      (function () ->
        apply1 translate_seq (expr, ch)) ;
      (function () ->
        apply1 translate_func (expr, ch)) ;
      (function () ->
        apply1 translate_rec_func (expr, ch)) ;
      (function () ->
        apply1 translate_call (expr, ch)) ;
      (function () ->
        apply1 translate_let (expr, ch)) ;
      (function () ->
        apply1 translate_neg (expr, ch)) ;
      (function () ->
        apply1 translate_add (expr, ch)) ;
      (function () ->
        apply1 translate_sub (expr, ch)) ;
      (function () ->
        apply1 translate_mul (expr, ch)) ;
      (function () ->
        apply1 translate_div (expr, ch)) ;
      (function () ->
        apply1 translate_pair (expr, ch)) ;
      (function () ->
        apply1 translate_fst (expr, ch)) ;
      (function () ->
        apply1 translate_snd (expr, ch)) ;
      (function () ->
        apply1 translate_left (expr, ch)) ;
      (function () ->
        apply1 translate_right (expr, ch)) ;
      (function () ->
        apply1 translate_match (expr, ch)) ;
      (function () ->
        apply1 translate_self (expr, ch)) ;
      (function () ->
        apply1 translate_receive (expr, ch)) ;
      (function () ->
        apply1 translate_spawn (expr, ch)) ;
      (function () ->
        apply1 translate_send (expr, ch))]
  and translate_add =
    function (expr, ch) ->
    begin match expr with
    | ActAdd (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (ChAdd (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_call =
    function (expr, ch) ->
    begin match expr with
    | ActCall (act_func, act_arg) ->
        let* name = apply1 string_unique_id () in
        let* ch_func = apply1 translate (act_func, ch) in
        let* ch_arg = apply1 translate (act_arg, ch) in
        M.ret (ChLet (name, ChCall (ch_func, ch_arg), ChCall (ChVar name, ChVar ch)))
    | _ -> M.fail ""
    end
  and translate_div =
    function (expr, ch) ->
    begin match expr with
    | ActDiv (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (ChDiv (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_fst =
    function (expr, ch) ->
    begin match expr with
    | ActFst act_pair ->
        let* ch_pair = apply1 translate (act_pair, ch) in
        M.ret (ChFst ch_pair)
    | _ -> M.fail ""
    end
  and translate_func =
    function (expr, ch) ->
    begin match expr with
    | ActFunc (param, act_body) ->
        let* ch_param = apply1 string_unique_id () in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (ChFunc (param, ChFunc (ch_param, ch_body)))
    | _ -> M.fail ""
    end
  and translate_left =
    function (expr, ch) ->
    begin match expr with
    | ActLeft act_cont ->
        let* ch_cont = apply1 translate (act_cont, ch) in
        M.ret (ChLeft ch_cont)
    | _ -> M.fail ""
    end
  and translate_let =
    function (expr, ch) ->
    begin match expr with
    | ActLet (param, act_arg, act_body) ->
        let* ch_arg = apply1 translate (act_arg, ch) in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (ChLet (param, ch_arg, ch_body))
    | _ -> M.fail ""
    end
  and translate_match =
    function (expr, ch) ->
    begin match expr with
    | ActMatch (act_arg, act_left, act_right) ->
        let* ch_arg = apply1 translate (act_arg, ch) in
        let* ch_left = apply1 translate (act_left, ch) in
        let* ch_right = apply1 translate (act_right, ch) in
        M.ret (ChMatch (ch_arg, ch_left, ch_right))
    | _ -> M.fail ""
    end
  and translate_mul =
    function (expr, ch) ->
    begin match expr with
    | ActMul (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (ChMul (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_neg =
    function (expr, ch) ->
    begin match expr with
    | ActNeg act_i ->
        let* ch_i = apply1 translate (act_i, ch) in
        M.ret (ChNeg ch_i)
    | _ -> M.fail ""
    end
  and translate_pair =
    function (expr, ch) ->
    begin match expr with
    | ActPair (act_fst, act_snd) ->
        let* ch_fst = apply1 translate (act_fst, ch) in
        let* ch_snd = apply1 translate (act_snd, ch) in
        M.ret (ChSeq (ch_fst, ch_snd))
    | _ -> M.fail ""
    end
  and translate_rec_func =
    function (expr, ch) ->
    begin match expr with
    | ActRecFunc (name, param, act_body) ->
        let* ch_param = apply1 string_unique_id () in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (ChRecFunc (name, param, ChFunc (ch_param, ch_body)))
    | _ -> M.fail ""
    end
  and translate_receive =
    function (expr, ch) ->
    begin match expr with
    | ActReceive -> M.ret (ChTake (ChVar ch))
    | _ -> M.fail ""
    end
  and translate_ret =
    function (expr, _) ->
    begin match expr with
    | ActRet value -> M.ret (ChRet value)
    | _ -> M.fail ""
    end
  and translate_right =
    function (expr, ch) ->
    begin match expr with
    | ActRight act_cont ->
        let* ch_cont = apply1 translate (act_cont, ch) in
        M.ret (ChRight ch_cont)
    | _ -> M.fail ""
    end
  and translate_self =
    function (expr, ch) ->
    begin match expr with
    | ActSelf -> M.ret (ChVar ch)
    | _ -> M.fail ""
    end
  and translate_send =
    function (expr, ch) ->
    begin match expr with
    | ActSend (act_msg, act_tgt) ->
        let* ch_msg = apply1 translate (act_msg, ch) in
        let* ch_tgt = apply1 translate (act_tgt, ch) in
        M.ret (ChGive (ch_tgt, ch_msg))
    | _ -> M.fail ""
    end
  and translate_seq =
    function (expr, ch) ->
    begin match expr with
    | ActSeq (act_a, act_b) ->
        let* ch_a = apply1 translate (act_a, ch) in
        let* ch_b = apply1 translate (act_b, ch) in
        M.ret (ChSeq (ch_a, ch_b))
    | _ -> M.fail ""
    end
  and translate_snd =
    function (expr, ch) ->
    begin match expr with
    | ActSnd act_pair ->
        let* ch_pair = apply1 translate (act_pair, ch) in
        M.ret (ChSnd ch_pair)
    | _ -> M.fail ""
    end
  and translate_spawn =
    function (expr, _) ->
    begin match expr with
    | ActSpawn act_expr ->
        let* new_ch = apply1 string_unique_id () in
        let* ch_expr = apply1 translate (act_expr, new_ch) in
        M.ret (ChLet (new_ch, ChNewCh, ChSeq (ChFork ch_expr, ChVar new_ch)))
    | _ -> M.fail ""
    end
  and translate_sub =
    function (expr, ch) ->
    begin match expr with
    | ActSub (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (ChSub (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_var =
    function (expr, _) ->
    begin match expr with
    | ActVar name -> M.ret (ChVar name)
    | _ -> M.fail ""
    end
end

module Interpreter = MakeInterpreter (Unspec)
