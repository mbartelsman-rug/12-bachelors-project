(** All types, and the unspecified terms *)
module type TYPES = sig

  type int_t
  and string_t
  and lit_t =
  | UnitVal
  | IntVal of int_t
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

  val body: unit -> expr_act_t M.t
  val string_unique_id: unit -> string_t M.t
  val string_of_expr_ch: expr_ch_t -> string
  val string_of_expr_act: expr_act_t -> string
  val expr_ch_of_string: string -> expr_ch_t
  val expr_act_of_string: string -> expr_act_t
  val string_of_value: lit_t -> string
  val value_of_string: string -> lit_t
end

(** A default instantiation *)
module Unspec = struct
  open Base
  include Types
  module M = Common.Monads.Identity


  let list_empty: expr_act_t =
    Left (Ret (UnitVal))

  let list_into (elem): expr_act_t =
    (Right (
      (Pair (
        elem,
        list_empty
      ))
    ))

  let list_concat: expr_act_t =
    (RecFunc ("__concat_concat", "__concat_args",
      (Let ("__concat_xs'", (Fst (Var ("__concat_args"))),
      (Let ("__concat_ys", (Snd (Var ("__concat_args"))),
      (Match ((Var "__concat_xs'"),
        (Func ("__concat_xs",
          (Var "__concat_ys"))),
        (Func ("__concat_xs",
          (Let ("__concat_hd", (Fst (Var "__concat_xs")),
          (Let ("__concat_tl", (Snd (Var "__concat_xs")),
          (Right (
            (Pair (
              (Var "__concat_hd"),
              (Call (
                (Var "__concat_concat"),
                (Pair ((Var "__concat_tl"),(Var "__concat_ys")))))))))))))))))))))))

  let drain: expr_act_t =
    (Func (
      "__drain_x",
      (Let ("__drain_vals", (Fst (Var "__drain_x")),
      (Let ("__drain_pids", (Snd (Var "__drain_x")),
      (Match (
        (Var "__drain_vals"),
        (Func (
          "__drain_vv",
          (Pair ((Var "__drain_vals"), (Var "__drain_pids"))))),
        (Func (
          "__drain_vv",
          (Let ("__drain_v", (Fst (Var "__drain_vv")),
          (Let ("__drain_vs", (Snd (Var "__drain_vv")),
          (Match (
            (Var "__drain_pids"),
            (Func (
              "__drain_pp",
              (Pair ((Var "__drain_vals"), (Var "__drain_pids"))))),
            (Func (
              "__drain_pp",
              (Let ("__drain_p", (Fst (Var "__drain_pp")),
              (Let ("__drain_ps", (Snd (Var "__drain_pp")),
              (Seq (
                (Send (
                  (Var "__drain_v"),
                  (Var "__drain_p"))),
                (Pair (
                  (Var "__drain_vs"),
                  (Var "__drain_ps")))))))))))))))))))))))))))
                  

  let body (): expr_act_t M.t =
    let expr: expr_act_t =
      RecFunc (
        "__body_this_func",
        "__body_state",
        (Let ("__body_in_val", (Receive),
        (Let ("__body_vals", (Fst (Var "__body_state")),
        (Let ("__body_pids", (Snd (Var "__body_state")),
        (Match ((Var "__body_in_val"),
          (Func ("__body_val",
            (Let ("__body_vals'", (Call (
                list_concat,
                (Pair ((Var "__body_vals"), list_into (Var "__body_val"))))),
            (Let ("__body_state'", (Call (
                drain,
                (Pair ((Var "__body_vals'"), (Var "__body_pids"))))),
            (Call ((Var "__body_this_func"), (Var "__body_state'"))))))))),
          (Func ("__body_pid",
            (Let ("__body_pids'",
            (Call (
                list_concat,
                (Pair ((Var "__body_pids"), list_into (Var "__body_pid"))))),
            (Let ("__body_state'",
              (Call (
                drain,
                (Pair ((Var "__body_vals"), (Var "__body_pids'"))))),
            (Call ((Var "__body_this_func"), (Var "__body_state'")))))))))))))))))) in
    M.ret expr


  let next_id =
    ref 0

  let string_unique_id () =
    let id = ! next_id in
    next_id := id + 1 ;
    Printf.sprintf "[%d]" id
    |> M.ret

  let string_of_expr_ch expr = sexp_of_expr_ch_t expr |> Common.Utilities.string_of_sexp
  let expr_ch_of_string strn = Core.Sexp.of_string strn |> expr_ch_t_of_sexp
  let string_of_expr_act expr = sexp_of_expr_act_t expr |> Common.Utilities.string_of_sexp
  let expr_act_of_string strn = Core.Sexp.of_string strn |> expr_act_t_of_sexp
  let string_of_value value = sexp_of_lit_t value |> Common.Utilities.string_of_sexp
  let value_of_string strn = Core.Sexp.of_string strn |> lit_t_of_sexp

end

(** The module type for interpreters *)
module type INTERPRETER = sig
  include UNSPEC
  module M: Common.Monads.MONAD

  val body: unit -> expr_act_t M.t
  val list_empty: unit -> expr_act_t M.t
  val string_unique_id: unit -> string_t M.t
  val translate: expr_ch_t -> expr_act_t M.t
  val translate_add: expr_ch_t -> expr_act_t M.t
  val translate_call: expr_ch_t -> expr_act_t M.t
  val translate_div: expr_ch_t -> expr_act_t M.t
  val translate_fork: expr_ch_t -> expr_act_t M.t
  val translate_fst: expr_ch_t -> expr_act_t M.t
  val translate_func: expr_ch_t -> expr_act_t M.t
  val translate_give: expr_ch_t -> expr_act_t M.t
  val translate_left: expr_ch_t -> expr_act_t M.t
  val translate_let: expr_ch_t -> expr_act_t M.t
  val translate_match: expr_ch_t -> expr_act_t M.t
  val translate_mul: expr_ch_t -> expr_act_t M.t
  val translate_neg: expr_ch_t -> expr_act_t M.t
  val translate_new_ch: expr_ch_t -> expr_act_t M.t
  val translate_pair: expr_ch_t -> expr_act_t M.t
  val translate_rec_func: expr_ch_t -> expr_act_t M.t
  val translate_ret: expr_ch_t -> expr_act_t M.t
  val translate_right: expr_ch_t -> expr_act_t M.t
  val translate_seq: expr_ch_t -> expr_act_t M.t
  val translate_snd: expr_ch_t -> expr_act_t M.t
  val translate_sub: expr_ch_t -> expr_act_t M.t
  val translate_take: expr_ch_t -> expr_act_t M.t
  val translate_var: expr_ch_t -> expr_act_t M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC): INTERPRETER = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec list_empty =
    function _ ->
    M.ret (Left (Ret UnitVal))
  and translate (expr: expr_ch_t): expr_act_t M.t =
    M.branch [
      (function () ->
        apply1 translate_ret expr) ;
      (function () ->
        apply1 translate_var expr) ;
      (function () ->
        apply1 translate_seq expr) ;
      (function () ->
        apply1 translate_func expr) ;
      (function () ->
        apply1 translate_rec_func expr) ;
      (function () ->
        apply1 translate_call expr) ;
      (function () ->
        apply1 translate_let expr) ;
      (function () ->
        apply1 translate_neg expr) ;
      (function () ->
        apply1 translate_add expr) ;
      (function () ->
        apply1 translate_sub expr) ;
      (function () ->
        apply1 translate_mul expr) ;
      (function () ->
        apply1 translate_div expr) ;
      (function () ->
        apply1 translate_pair expr) ;
      (function () ->
        apply1 translate_fst expr) ;
      (function () ->
        apply1 translate_snd expr) ;
      (function () ->
        apply1 translate_left expr) ;
      (function () ->
        apply1 translate_right expr) ;
      (function () ->
        apply1 translate_match expr) ;
      (function () ->
        apply1 translate_new_ch expr) ;
      (function () ->
        apply1 translate_give expr) ;
      (function () ->
        apply1 translate_take expr) ;
      (function () ->
        apply1 translate_fork expr)]
  and translate_add expr =
    begin match expr with
    | Add (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (Add (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_call expr =
    begin match expr with
    | Call (ch_func, ch_arg) ->
        let* act_func = apply1 translate ch_func in
        let* act_arg = apply1 translate ch_arg in
        M.ret (Call (act_func, act_arg))
    | _ -> M.fail ""
    end
  and translate_div expr =
    begin match expr with
    | Div (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (Div (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_fork expr =
    begin match expr with
    | Fork ch_expr ->
        let* new_act = apply1 string_unique_id () in
        let* act_expr = apply1 translate ch_expr in
        M.ret (Let (new_act, Spawn act_expr, Ret UnitVal))
    | _ -> M.fail ""
    end
  and translate_fst expr =
    begin match expr with
    | Fst ch_pair ->
        let* act_pair = apply1 translate ch_pair in
        M.ret (Fst act_pair)
    | _ -> M.fail ""
    end
  and translate_func expr =
    begin match expr with
    | Func (param, ch_body) ->
        let* act_body = apply1 translate ch_body in
        M.ret (Func (param, act_body))
    | _ -> M.fail ""
    end
  and translate_give expr =
    begin match expr with
    | Give (ch_tgt, ch_msg) ->
        let* act_msg = apply1 translate ch_msg in
        let* act_tgt = apply1 translate ch_tgt in
        M.ret (Send (Left act_msg, act_tgt))
    | _ -> M.fail ""
    end
  and translate_left expr =
    begin match expr with
    | Left ch_cont ->
        let* act_cont = apply1 translate ch_cont in
        M.ret (Left act_cont)
    | _ -> M.fail ""
    end
  and translate_let expr =
    begin match expr with
    | Let (param, ch_arg, ch_body) ->
        let* act_arg = apply1 translate ch_arg in
        let* act_body = apply1 translate ch_body in
        M.ret (Let (param, act_arg, act_body))
    | _ -> M.fail ""
    end
  and translate_match expr =
    begin match expr with
    | Match (ch_arg, ch_left, ch_right) ->
        let* act_arg = apply1 translate ch_arg in
        let* act_left = apply1 translate ch_left in
        let* act_right = apply1 translate ch_right in
        M.ret (Match (act_arg, act_left, act_right))
    | _ -> M.fail ""
    end
  and translate_mul expr =
    begin match expr with
    | Mul (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (Mul (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_neg expr =
    begin match expr with
    | Neg ch_i ->
        let* act_i = apply1 translate ch_i in
        M.ret (Neg act_i)
    | _ -> M.fail ""
    end
  and translate_new_ch expr =
    begin match expr with
    | NewCh ->
        let* body_expr = apply1 body () in
        let* list_1 = apply1 list_empty () in
        let* list_2 = apply1 list_empty () in
        M.ret (Spawn (Call (body_expr, Pair (list_1, list_2))))
    | _ -> M.fail ""
    end
  and translate_pair expr =
    begin match expr with
    | Pair (ch_fst, ch_snd) ->
        let* act_fst = apply1 translate ch_fst in
        let* act_snd = apply1 translate ch_snd in
        M.ret (Seq (act_fst, act_snd))
    | _ -> M.fail ""
    end
  and translate_rec_func expr =
    begin match expr with
    | RecFunc (name, param, ch_body) ->
        let* act_body = apply1 translate ch_body in
        M.ret (RecFunc (name, param, act_body))
    | _ -> M.fail ""
    end
  and translate_ret expr =
    begin match expr with
    | Ret value -> M.ret (Ret value)
    | _ -> M.fail ""
    end
  and translate_right expr =
    begin match expr with
    | Right ch_cont ->
        let* act_cont = apply1 translate ch_cont in
        M.ret (Right act_cont)
    | _ -> M.fail ""
    end
  and translate_seq expr =
    begin match expr with
    | Seq (ch_a, ch_b) ->
        let* act_a = apply1 translate ch_a in
        let* act_b = apply1 translate ch_b in
        M.ret (Seq (act_a, act_b))
    | _ -> M.fail ""
    end
  and translate_snd expr =
    begin match expr with
    | Snd ch_pair ->
        let* act_pair = apply1 translate ch_pair in
        M.ret (Snd act_pair)
    | _ -> M.fail ""
    end
  and translate_sub expr =
    begin match expr with
    | Sub (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (Sub (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_take expr =
    begin match expr with
    | Take ch_tgt ->
        let* act_tgt = apply1 translate ch_tgt in
        let* self_pid = apply1 string_unique_id () in
        M.ret (Let (self_pid, Self, Seq (Send (Right (Var self_pid), act_tgt), Receive)))
    | _ -> M.fail ""
    end
  and translate_var expr =
    begin match expr with
    | Var name -> M.ret (Var name)
    | _ -> M.fail ""
    end
end

module Interpreter: INTERPRETER = MakeInterpreter (Unspec)
