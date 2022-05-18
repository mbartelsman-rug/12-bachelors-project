(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: Monads.MONAD

  type env
  type literal
  type name
  type vint

  type vunion =
  | VRight of value
  | VLeft of value
  and value =
  | VUnit
  | VUnion of vunion
  | VPair of (value * value)
  | VInt of vint
  | VFun of (name * expr * env)
  and expr =
  | Var of name
  | Unit
  | Seq of (expr * expr)
  | Second of expr
  | Right of expr
  | Ret of expr
  | Pair of (expr * expr)
  | Neg of expr
  | Match of (expr * expr * expr)
  | Lit of literal
  | Let of (name * expr * expr)
  | Left of expr
  | Fun of (name * expr)
  | First of expr
  | App of (expr * expr)
  | Add of (expr * expr)

  val add_int: vint * vint -> value M.t
  val env_get: env * name -> value M.t
  val env_store: env * name * value -> env M.t
  val eval: env * expr -> value M.t
  val eval_add: env * expr -> value M.t
  val eval_app: env * expr -> value M.t
  val eval_first: env * expr -> value M.t
  val eval_fun: env * expr -> value M.t
  val eval_left: env * expr -> value M.t
  val eval_let: env * expr -> value M.t
  val eval_lit: env * expr -> value M.t
  val eval_match: env * expr -> value M.t
  val eval_neg: env * expr -> value M.t
  val eval_pair: env * expr -> value M.t
  val eval_ret: env * expr -> value M.t
  val eval_right: env * expr -> value M.t
  val eval_second: env * expr -> value M.t
  val eval_seq: env * expr -> value M.t
  val eval_unit: env * expr -> value M.t
  val eval_var: env * expr -> value M.t
  val int_of_value: value -> vint M.t
  val neg_int: vint -> value M.t
  val value_of_literal: literal -> value M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: Impls.UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply

  let rec eval =
    function (s, e) ->
    M.branch [
      (function () ->
        apply1 eval_unit (s, e)) ;
      (function () ->
        apply1 eval_seq (s, e)) ;
      (function () ->
        apply1 eval_let (s, e)) ;
      (function () ->
        apply1 eval_ret (s, e)) ;
      (function () ->
        apply1 eval_fun (s, e)) ;
      (function () ->
        apply1 eval_app (s, e)) ;
      (function () ->
        apply1 eval_var (s, e)) ;
      (function () ->
        apply1 eval_lit (s, e)) ;
      (function () ->
        apply1 eval_neg (s, e)) ;
      (function () ->
        apply1 eval_add (s, e)) ;
      (function () ->
        apply1 eval_pair (s, e)) ;
      (function () ->
        apply1 eval_first (s, e)) ;
      (function () ->
        apply1 eval_second (s, e)) ;
      (function () ->
        apply1 eval_left (s, e)) ;
      (function () ->
        apply1 eval_right (s, e)) ;
      (function () ->
        apply1 eval_match (s, e))]
  and eval_add =
    function (s, e) ->
    begin match e with
    | Add (a, b) ->
        let* a' = apply1 eval (s, a) in
        let* b' = apply1 eval (s, b) in
        let* i = apply1 int_of_value a' in
        let* j = apply1 int_of_value b' in
        apply1 add_int (i, j)
    | _ -> M.fail ""
    end
  and eval_app =
    function (s, e) ->
    begin match e with
    | App (f, v) ->
        let* _tmp = apply1 eval (s, f) in
        begin match _tmp with
        | VFun (n, b, z) ->
            let* v' = apply1 eval (s, v) in
            let* z' = apply1 env_store (z, n, v') in
            apply1 eval (z', b)
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_first =
    function (s, e) ->
    begin match e with
    | First p ->
        let* _tmp = apply1 eval (s, p) in
        begin match _tmp with
        | VPair (fst, _) -> M.ret fst
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_fun =
    function (s, e) ->
    begin match e with
    | Fun (n, b) -> M.ret (VFun (n, b, s))
    | _ -> M.fail ""
    end
  and eval_left =
    function (s, e) ->
    begin match e with
    | Left e' ->
        let* e'' = apply1 eval (s, e') in
        M.ret (VUnion (VLeft e''))
    | _ -> M.fail ""
    end
  and eval_let =
    function (s, e) ->
    begin match e with
    | Let (n, v, b) ->
        let* v' = apply1 eval (s, v) in
        let* s' = apply1 env_store (s, n, v') in
        apply1 eval (s', b)
    | _ -> M.fail ""
    end
  and eval_lit =
    function (_, e) ->
    begin match e with
    | Lit l -> apply1 value_of_literal l
    | _ -> M.fail ""
    end
  and eval_match =
    function (s, e) ->
    begin match e with
    | Match (g, on_t, on_f) ->
        let* _tmp = apply1 eval (s, g) in
        begin match _tmp with
        | VUnion g' ->
            M.branch [
              (function () ->
                begin match g' with
                | VLeft v ->
                    let* _tmp = apply1 eval (s, on_t) in
                    begin match _tmp with
                    | VFun (n, b, z) ->
                        let* z' = apply1 env_store (z, n, v) in
                        apply1 eval (z', b)
                    | _ -> M.fail ""
                    end
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match g' with
                | VRight v ->
                    let* _tmp = apply1 eval (s, on_f) in
                    begin match _tmp with
                    | VFun (n, b, z) ->
                        let* z' = apply1 env_store (z, n, v) in
                        apply1 eval (z', b)
                    | _ -> M.fail ""
                    end
                | _ -> M.fail ""
                end)]
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_neg =
    function (s, e) ->
    begin match e with
    | Neg e' ->
        let* e'' = apply1 eval (s, e') in
        let* i = apply1 int_of_value e'' in
        apply1 neg_int i
    | _ -> M.fail ""
    end
  and eval_pair =
    function (s, e) ->
    begin match e with
    | Pair (a, b) ->
        let* a' = apply1 eval (s, a) in
        let* b' = apply1 eval (s, b) in
        M.ret (VPair (a', b'))
    | _ -> M.fail ""
    end
  and eval_ret =
    function (s, e) ->
    begin match e with
    | Ret r -> apply1 eval (s, r)
    | _ -> M.fail ""
    end
  and eval_right =
    function (s, e) ->
    begin match e with
    | Right e' ->
        let* e'' = apply1 eval (s, e') in
        M.ret (VUnion (VRight e''))
    | _ -> M.fail ""
    end
  and eval_second =
    function (s, e) ->
    begin match e with
    | Second p ->
        let* _tmp = apply1 eval (s, p) in
        begin match _tmp with
        | VPair (_, snd) -> M.ret snd
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_seq =
    function (s, e) ->
    begin match e with
    | Seq (a, b) ->
        let* _ = apply1 eval (s, a) in
        apply1 eval (s, b)
    | _ -> M.fail ""
    end
  and eval_unit =
    function (_, e) ->
    begin match e with
    | Unit -> M.ret VUnit
    | _ -> M.fail ""
    end
  and eval_var =
    function (s, e) ->
    begin match e with
    | Var n -> apply1 env_get (s, n)
    | _ -> M.fail ""
    end
end

(* Main interpreter *)
module Main = MakeInterpreter (Impls.Main)
