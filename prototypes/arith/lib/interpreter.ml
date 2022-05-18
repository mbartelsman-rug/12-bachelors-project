open Monad
open Unspec

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type literal
  type value

  type monop =
  | Neg
  and expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Add

  val add: value -> (value -> value M.t) M.t
  val apply: binop -> (value -> (value -> value M.t) M.t) M.t
  val eval: expr -> value M.t
  val litToVal: literal -> value M.t
  val sub: value -> (value -> value M.t) M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2
  let apply3 f arg1 arg2 arg3 =
    let* _tmp = apply1 f arg1 in
    apply2 _tmp arg2 arg3

  let rec apply op =
    M.branch [
      (function () ->
        begin match op with
        | Add -> M.ret add
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match op with
        | Sub -> M.ret sub
        | _ -> M.fail ""
        end)]
  and eval e =
    M.branch [
      (function () ->
        begin match e with
        | Const lit -> apply1 litToVal lit
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match e with
        | Binop (op, lhs, rhs) ->
            let* lhs_res = apply1 eval lhs in
            let* rhs_res = apply1 eval rhs in
            apply3 apply op lhs_res rhs_res
        | _ -> M.fail ""
        end)]
end

module Interpreter = MakeInterpreter (Unspec)
