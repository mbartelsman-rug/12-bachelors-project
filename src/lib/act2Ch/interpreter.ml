module type TYPES = sig

  type int_t
  and string_t
  and lit_t =
  | UnitVal
  | IntVal of int_t
  and expr_act_t =
  | Var of string_t
  | Sub of (expr_act_t * expr_act_t)
  | Spawn of expr_act_t
  | Snd of expr_act_t
  | Seq of (expr_act_t * expr_act_t)
  | Send of (expr_act_t * expr_act_t)
  | Self
  | Right of expr_act_t
  | Ret of lit_t
  | Receive
  | RecFunc of (string_t * string_t * expr_act_t)
  | Pair of (expr_act_t * expr_act_t)
  | Neg of expr_act_t
  | Mul of (expr_act_t * expr_act_t)
  | Match of (expr_act_t * expr_act_t * expr_act_t)
  | Let of (string_t * expr_act_t * expr_act_t)
  | Left of expr_act_t
  | Func of (string_t * expr_act_t)
  | Fst of expr_act_t
  | Div of (expr_act_t * expr_act_t)
  | Call of (expr_act_t * expr_act_t)
  | Add of (expr_act_t * expr_act_t)
  and expr_ch_t =
  | Var of string_t
  | Take of expr_ch_t
  | Sub of (expr_ch_t * expr_ch_t)
  | Snd of expr_ch_t
  | Seq of (expr_ch_t * expr_ch_t)
  | Right of expr_ch_t
  | Ret of lit_t
  | RecFunc of (string_t * string_t * expr_ch_t)
  | Pair of (expr_ch_t * expr_ch_t)
  | NewCh
  | Neg of expr_ch_t
  | Mul of (expr_ch_t * expr_ch_t)
  | Match of (expr_ch_t * expr_ch_t * expr_ch_t)
  | Let of (string_t * expr_ch_t * expr_ch_t)
  | Left of expr_ch_t
  | Give of (expr_ch_t * expr_ch_t)
  | Func of (string_t * expr_ch_t)
  | Fst of expr_ch_t
  | Fork of expr_ch_t
  | Div of (expr_ch_t * expr_ch_t)
  | Call of (expr_ch_t * expr_ch_t)
  | Add of (expr_ch_t * expr_ch_t)
  val make_name: string -> string_t
  val make_int: int -> lit_t
end

module Types = struct
  open Base
  
  type int_t = Int.t
  and string_t = String.t
  and lit_t =
  | UnitVal
  | IntVal of int_t
  and expr_act_t =
  | Var of string_t
  | Sub of (expr_act_t * expr_act_t)
  | Spawn of expr_act_t
  | Snd of expr_act_t
  | Seq of (expr_act_t * expr_act_t)
  | Send of (expr_act_t * expr_act_t)
  | Self
  | Right of expr_act_t
  | Ret of lit_t
  | Receive
  | RecFunc of (string_t * string_t * expr_act_t)
  | Pair of (expr_act_t * expr_act_t)
  | Neg of expr_act_t
  | Mul of (expr_act_t * expr_act_t)
  | Match of (expr_act_t * expr_act_t * expr_act_t)
  | Let of (string_t * expr_act_t * expr_act_t)
  | Left of expr_act_t
  | Func of (string_t * expr_act_t)
  | Fst of expr_act_t
  | Div of (expr_act_t * expr_act_t)
  | Call of (expr_act_t * expr_act_t)
  | Add of (expr_act_t * expr_act_t)
  [@@deriving sexp]
  and expr_ch_t =
  | Var of string_t
  | Take of expr_ch_t
  | Sub of (expr_ch_t * expr_ch_t)
  | Snd of expr_ch_t
  | Seq of (expr_ch_t * expr_ch_t)
  | Right of expr_ch_t
  | Ret of lit_t
  | RecFunc of (string_t * string_t * expr_ch_t)
  | Pair of (expr_ch_t * expr_ch_t)
  | NewCh
  | Neg of expr_ch_t
  | Mul of (expr_ch_t * expr_ch_t)
  | Match of (expr_ch_t * expr_ch_t * expr_ch_t)
  | Let of (string_t * expr_ch_t * expr_ch_t)
  | Left of expr_ch_t
  | Give of (expr_ch_t * expr_ch_t)
  | Func of (string_t * expr_ch_t)
  | Fst of expr_ch_t
  | Fork of expr_ch_t
  | Div of (expr_ch_t * expr_ch_t)
  | Call of (expr_ch_t * expr_ch_t)
  | Add of (expr_ch_t * expr_ch_t)
  [@@deriving sexp]

  let make_name s = s

  let make_int i = IntVal i
end

module type UNSPEC = sig
  include TYPES
  module M: Common.Monads.MONAD
  val string_of_expr_ch: expr_ch_t -> string
  val string_of_expr_act: expr_act_t -> string
  val expr_ch_of_string: string -> expr_ch_t
  val expr_act_of_string: string -> expr_act_t
  val string_of_value: lit_t -> string
  val value_of_string: string -> lit_t
  val string_unique_id: unit -> string_t M.t
end

(** A default instantiation *)
module Unspec = struct
  open Base
  include Types
  module M = Common.Monads.Identity
  
  let string_of_expr_ch expr = sexp_of_expr_ch_t expr |> Common.Utilities.string_of_sexp
  let expr_ch_of_string strn = Core.Sexp.of_string strn |> expr_ch_t_of_sexp
  let string_of_expr_act expr = sexp_of_expr_act_t expr |> Common.Utilities.string_of_sexp
  let expr_act_of_string strn = Core.Sexp.of_string strn |> expr_act_t_of_sexp
  let string_of_value value = sexp_of_lit_t value |> Common.Utilities.string_of_sexp
  let value_of_string strn = Core.Sexp.of_string strn |> lit_t_of_sexp

  let next_id =
    ref 0

  let string_unique_id () =
    let id = ! next_id in
    next_id := id + 1 ;
    Printf.sprintf "[%d]" id
    |> M.ret
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  include UNSPEC

  val translate: expr_act_t * string_t -> expr_ch_t M.t
  val translate_add: expr_act_t * string_t -> expr_ch_t M.t
  val translate_call: expr_act_t * string_t -> expr_ch_t M.t
  val translate_div: expr_act_t * string_t -> expr_ch_t M.t
  val translate_fst: expr_act_t * string_t -> expr_ch_t M.t
  val translate_func: expr_act_t * string_t -> expr_ch_t M.t
  val translate_left: expr_act_t * string_t -> expr_ch_t M.t
  val translate_let: expr_act_t * string_t -> expr_ch_t M.t
  val translate_match: expr_act_t * string_t -> expr_ch_t M.t
  val translate_mul: expr_act_t * string_t -> expr_ch_t M.t
  val translate_neg: expr_act_t * string_t -> expr_ch_t M.t
  val translate_pair: expr_act_t * string_t -> expr_ch_t M.t
  val translate_rec_func: expr_act_t * string_t -> expr_ch_t M.t
  val translate_receive: expr_act_t * string_t -> expr_ch_t M.t
  val translate_ret: expr_act_t * string_t -> expr_ch_t M.t
  val translate_right: expr_act_t * string_t -> expr_ch_t M.t
  val translate_self: expr_act_t * string_t -> expr_ch_t M.t
  val translate_send: expr_act_t * string_t -> expr_ch_t M.t
  val translate_seq: expr_act_t * string_t -> expr_ch_t M.t
  val translate_snd: expr_act_t * string_t -> expr_ch_t M.t
  val translate_spawn: expr_act_t * string_t -> expr_ch_t M.t
  val translate_sub: expr_act_t * string_t -> expr_ch_t M.t
  val translate_var: expr_act_t * string_t -> expr_ch_t M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec translate: expr_act_t * string_t -> expr_ch_t M.t =
    function ((expr, ch): (expr_act_t * string_t)) ->
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
    | Add (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (Add (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_call =
    function (expr, ch) ->
    begin match expr with
    | Call (act_func, act_arg) ->
        let* name = apply1 string_unique_id () in
        let* ch_func = apply1 translate (act_func, ch) in
        let* ch_arg = apply1 translate (act_arg, ch) in
        M.ret (Let (name, Call (ch_func, ch_arg), Call (Var name, Var ch)))
    | _ -> M.fail ""
    end
  and translate_div =
    function (expr, ch) ->
    begin match expr with
    | Div (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (Div (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_fst =
    function (expr, ch) ->
    begin match expr with
    | Fst act_pair ->
        let* ch_pair = apply1 translate (act_pair, ch) in
        M.ret (Fst ch_pair)
    | _ -> M.fail ""
    end
  and translate_func =
    function (expr, ch) ->
    begin match expr with
    | Func (param, act_body) ->
        let* ch_param = apply1 string_unique_id () in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (Func (param, Func (ch_param, ch_body)))
    | _ -> M.fail ""
    end
  and translate_left =
    function (expr, ch) ->
    begin match expr with
    | Left act_cont ->
        let* ch_cont = apply1 translate (act_cont, ch) in
        M.ret (Left ch_cont)
    | _ -> M.fail ""
    end
  and translate_let =
    function (expr, ch) ->
    begin match expr with
    | Let (param, act_arg, act_body) ->
        let* ch_arg = apply1 translate (act_arg, ch) in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (Let (param, ch_arg, ch_body))
    | _ -> M.fail ""
    end
  and translate_match =
    function (expr, ch) ->
    begin match expr with
    | Match (act_arg, act_left, act_right) ->
        let* ch_arg = apply1 translate (act_arg, ch) in
        let* ch_left = apply1 translate (act_left, ch) in
        let* ch_right = apply1 translate (act_right, ch) in
        M.ret (Match (ch_arg, ch_left, ch_right))
    | _ -> M.fail ""
    end
  and translate_mul =
    function (expr, ch) ->
    begin match expr with
    | Mul (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (Mul (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_neg =
    function (expr, ch) ->
    begin match expr with
    | Neg act_i ->
        let* ch_i = apply1 translate (act_i, ch) in
        M.ret (Neg ch_i)
    | _ -> M.fail ""
    end
  and translate_pair =
    function (expr, ch) ->
    begin match expr with
    | Pair (act_fst, act_snd) ->
        let* ch_fst = apply1 translate (act_fst, ch) in
        let* ch_snd = apply1 translate (act_snd, ch) in
        M.ret (Seq (ch_fst, ch_snd))
    | _ -> M.fail ""
    end
  and translate_rec_func =
    function (expr, ch) ->
    begin match expr with
    | RecFunc (name, param, act_body) ->
        let* ch_param = apply1 string_unique_id () in
        let* ch_body = apply1 translate (act_body, ch) in
        M.ret (RecFunc (name, param, Func (ch_param, ch_body)))
    | _ -> M.fail ""
    end
  and translate_receive =
    function (expr, ch) ->
    begin match expr with
    | Receive -> M.ret (Take (Var ch))
    | _ -> M.fail ""
    end
  and translate_ret =
    function (expr, _) ->
    begin match expr with
    | Ret value -> M.ret (Ret value)
    | _ -> M.fail ""
    end
  and translate_right =
    function (expr, ch) ->
    begin match expr with
    | Right act_cont ->
        let* ch_cont = apply1 translate (act_cont, ch) in
        M.ret (Right ch_cont)
    | _ -> M.fail ""
    end
  and translate_self =
    function (expr, ch) ->
    begin match expr with
    | Self -> M.ret (Var ch)
    | _ -> M.fail ""
    end
  and translate_send =
    function (expr, ch) ->
    begin match expr with
    | Send (act_msg, act_tgt) ->
        let* ch_msg = apply1 translate (act_msg, ch) in
        let* ch_tgt = apply1 translate (act_tgt, ch) in
        M.ret (Give (ch_tgt, ch_msg))
    | _ -> M.fail ""
    end
  and translate_seq =
    function (expr, ch) ->
    begin match expr with
    | Seq (act_a, act_b) ->
        let* ch_a = apply1 translate (act_a, ch) in
        let* ch_b = apply1 translate (act_b, ch) in
        M.ret (Seq (ch_a, ch_b))
    | _ -> M.fail ""
    end
  and translate_snd =
    function (expr, ch) ->
    begin match expr with
    | Snd act_pair ->
        let* ch_pair = apply1 translate (act_pair, ch) in
        M.ret (Snd ch_pair)
    | _ -> M.fail ""
    end
  and translate_spawn =
    function (expr, _) ->
    begin match expr with
    | Spawn act_expr ->
        let* new_ch = apply1 string_unique_id () in
        let* ch_expr = apply1 translate (act_expr, new_ch) in
        M.ret (Let (new_ch, NewCh, Seq (Fork ch_expr, Var new_ch)))
    | _ -> M.fail ""
    end
  and translate_sub =
    function (expr, ch) ->
    begin match expr with
    | Sub (act_i, act_j) ->
        let* ch_i = apply1 translate (act_i, ch) in
        let* ch_j = apply1 translate (act_j, ch) in
        M.ret (Sub (ch_i, ch_j))
    | _ -> M.fail ""
    end
  and translate_var =
    function (expr, _) ->
    begin match expr with
    | Var name -> M.ret (Var name)
    | _ -> M.fail ""
    end
end

module Interpreter = MakeInterpreter (Unspec)
