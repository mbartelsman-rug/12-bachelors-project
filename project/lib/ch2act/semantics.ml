(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type int_t
  type string_t
end

(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end

(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: MONAD
  include TYPES

  type lit_t =
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

  val body: unit -> expr_act_t M.t
  val string_unique_id: unit -> string_t M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type lit_t =
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

  let body _ = raise (NotImplemented "body")
  let string_unique_id _ = raise (NotImplemented "string_unique_id")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type int_t
  type string_t

  type lit_t =
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
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec list_empty =
    function _ ->
    M.ret (ActLeft (ActRet UnitVal))
  and translate expr =
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
    | ChAdd (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (ActAdd (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_call expr =
    begin match expr with
    | ChCall (ch_func, ch_arg) ->
        let* act_func = apply1 translate ch_func in
        let* act_arg = apply1 translate ch_arg in
        M.ret (ActCall (act_func, act_arg))
    | _ -> M.fail ""
    end
  and translate_div expr =
    begin match expr with
    | ChDiv (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (ActDiv (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_fork expr =
    begin match expr with
    | ChFork ch_expr ->
        let* new_act = apply1 string_unique_id () in
        let* act_expr = apply1 translate ch_expr in
        M.ret (ActLet (new_act, act_expr, ActRet UnitVal))
    | _ -> M.fail ""
    end
  and translate_fst expr =
    begin match expr with
    | ChFst ch_pair ->
        let* act_pair = apply1 translate ch_pair in
        M.ret (ActFst act_pair)
    | _ -> M.fail ""
    end
  and translate_func expr =
    begin match expr with
    | ChFunc (param, ch_body) ->
        let* act_body = apply1 translate ch_body in
        M.ret (ActFunc (param, act_body))
    | _ -> M.fail ""
    end
  and translate_give expr =
    begin match expr with
    | ChGive (ch_tgt, ch_msg) ->
        let* act_msg = apply1 translate ch_msg in
        let* act_tgt = apply1 translate ch_tgt in
        M.ret (ActSend (ActLeft act_msg, act_tgt))
    | _ -> M.fail ""
    end
  and translate_left expr =
    begin match expr with
    | ChLeft ch_cont ->
        let* act_cont = apply1 translate ch_cont in
        M.ret (ActLeft act_cont)
    | _ -> M.fail ""
    end
  and translate_let expr =
    begin match expr with
    | ChLet (param, ch_arg, ch_body) ->
        let* act_arg = apply1 translate ch_arg in
        let* act_body = apply1 translate ch_body in
        M.ret (ActLet (param, act_arg, act_body))
    | _ -> M.fail ""
    end
  and translate_match expr =
    begin match expr with
    | ChMatch (ch_arg, ch_left, ch_right) ->
        let* act_arg = apply1 translate ch_arg in
        let* act_left = apply1 translate ch_left in
        let* act_right = apply1 translate ch_right in
        M.ret (ActMatch (act_arg, act_left, act_right))
    | _ -> M.fail ""
    end
  and translate_mul expr =
    begin match expr with
    | ChMul (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (ActMul (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_neg expr =
    begin match expr with
    | ChNeg ch_i ->
        let* act_i = apply1 translate ch_i in
        M.ret (ActNeg act_i)
    | _ -> M.fail ""
    end
  and translate_new_ch expr =
    begin match expr with
    | ChNewCh ->
        let* body_expr = apply1 body () in
        M.ret (ActSpawn (ActCall (body_expr, ActPair (ActLeft (ActRet UnitVal), ActLeft (ActRet UnitVal)))))
    | _ -> M.fail ""
    end
  and translate_pair expr =
    begin match expr with
    | ChPair (ch_fst, ch_snd) ->
        let* act_fst = apply1 translate ch_fst in
        let* act_snd = apply1 translate ch_snd in
        M.ret (ActSeq (act_fst, act_snd))
    | _ -> M.fail ""
    end
  and translate_rec_func expr =
    begin match expr with
    | ChRecFunc (name, param, ch_body) ->
        let* act_body = apply1 translate ch_body in
        M.ret (ActRecFunc (name, param, act_body))
    | _ -> M.fail ""
    end
  and translate_ret expr =
    begin match expr with
    | ChRet value -> M.ret (ActRet value)
    | _ -> M.fail ""
    end
  and translate_right expr =
    begin match expr with
    | ChRight ch_cont ->
        let* act_cont = apply1 translate ch_cont in
        M.ret (ActRight act_cont)
    | _ -> M.fail ""
    end
  and translate_seq expr =
    begin match expr with
    | ChSeq (ch_a, ch_b) ->
        let* act_a = apply1 translate ch_a in
        let* act_b = apply1 translate ch_b in
        M.ret (ActSeq (act_a, act_b))
    | _ -> M.fail ""
    end
  and translate_snd expr =
    begin match expr with
    | ChSnd ch_pair ->
        let* act_pair = apply1 translate ch_pair in
        M.ret (ActSnd act_pair)
    | _ -> M.fail ""
    end
  and translate_sub expr =
    begin match expr with
    | ChSub (ch_i, ch_j) ->
        let* act_i = apply1 translate ch_i in
        let* act_j = apply1 translate ch_j in
        M.ret (ActSub (act_i, act_j))
    | _ -> M.fail ""
    end
  and translate_take expr =
    begin match expr with
    | ChTake ch_tgt ->
        let* act_tgt = apply1 translate ch_tgt in
        let* self_pid = apply1 string_unique_id () in
        M.ret (ActLet (self_pid, ActSelf, ActSeq (ActSend (ActRight (ActVar self_pid), act_tgt), ActReceive)))
    | _ -> M.fail ""
    end
  and translate_var expr =
    begin match expr with
    | ChVar name -> M.ret (ActVar name)
    | _ -> M.fail ""
    end
end