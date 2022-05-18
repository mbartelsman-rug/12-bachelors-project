(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type env
  type literal
  type name
  type v_int
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

  type value =
  | VUnit
  | VPair of (value * value)
  | VInt of v_int
  | VFun of (name * expr * env)
  | VEither of v_either
  and v_either =
  | VTrue of value
  | VFalse of value
  and expr =
  | Var of name
  | Unit
  | True of expr
  | Snd of expr
  | Seq of (expr * expr)
  | Ret of expr
  | Pair of (expr * expr)
  | Neg of expr
  | Match of (expr * expr * expr)
  | Lit of literal
  | Let of (name * expr * expr)
  | Fun of (name * expr)
  | Fst of expr
  | False of expr
  | App of (expr * expr)
  | Add of (expr * expr)

  val add_int: v_int * v_int -> value M.t
  val env_read: env * name -> value M.t
  val env_write: env * name * value -> env M.t
  val int_of_value: value -> v_int M.t
  val neg_int: v_int -> value M.t
  val value_of_literal: literal -> value M.t
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type value =
  | VUnit
  | VPair of (value * value)
  | VInt of v_int
  | VFun of (name * expr * env)
  | VEither of v_either
  and v_either =
  | VTrue of value
  | VFalse of value
  and expr =
  | Var of name
  | Unit
  | True of expr
  | Snd of expr
  | Seq of (expr * expr)
  | Ret of expr
  | Pair of (expr * expr)
  | Neg of expr
  | Match of (expr * expr * expr)
  | Lit of literal
  | Let of (name * expr * expr)
  | Fun of (name * expr)
  | Fst of expr
  | False of expr
  | App of (expr * expr)
  | Add of (expr * expr)

  let add_int _ = raise (NotImplemented "add_int")
  let env_read _ = raise (NotImplemented "env_read")
  let env_write _ = raise (NotImplemented "env_write")
  let int_of_value _ = raise (NotImplemented "int_of_value")
  let neg_int _ = raise (NotImplemented "neg_int")
  let value_of_literal _ = raise (NotImplemented "value_of_literal")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type env
  type literal
  type name
  type v_int

  type value =
  | VUnit
  | VPair of (value * value)
  | VInt of v_int
  | VFun of (name * expr * env)
  | VEither of v_either
  and v_either =
  | VTrue of value
  | VFalse of value
  and expr =
  | Var of name
  | Unit
  | True of expr
  | Snd of expr
  | Seq of (expr * expr)
  | Ret of expr
  | Pair of (expr * expr)
  | Neg of expr
  | Match of (expr * expr * expr)
  | Lit of literal
  | Let of (name * expr * expr)
  | Fun of (name * expr)
  | Fst of expr
  | False of expr
  | App of (expr * expr)
  | Add of (expr * expr)

  val add_int: v_int * v_int -> value M.t
  val env_read: env * name -> value M.t
  val env_write: env * name * value -> env M.t
  val eval: env * expr -> value M.t
  val eval_add: env * expr -> value M.t
  val eval_app: env * expr -> value M.t
  val eval_false: env * expr -> value M.t
  val eval_fst: env * expr -> value M.t
  val eval_fun: env * expr -> value M.t
  val eval_let: env * expr -> value M.t
  val eval_lit: env * expr -> value M.t
  val eval_match: env * expr -> value M.t
  val eval_neg: env * expr -> value M.t
  val eval_pair: env * expr -> value M.t
  val eval_ret: env * expr -> value M.t
  val eval_seq: env * expr -> value M.t
  val eval_snd: env * expr -> value M.t
  val eval_true: env * expr -> value M.t
  val eval_unit: env * expr -> value M.t
  val eval_var: env * expr -> value M.t
  val int_of_value: value -> v_int M.t
  val neg_int: v_int -> value M.t
  val value_of_literal: literal -> value M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
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
        apply1 eval_fst (s, e)) ;
      (function () ->
        apply1 eval_snd (s, e)) ;
      (function () ->
        apply1 eval_true (s, e)) ;
      (function () ->
        apply1 eval_false (s, e)) ;
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
            let* z' = apply1 env_write (z, n, v') in
            apply1 eval (z', b)
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_false =
    function (s, e) ->
    begin match e with
    | False e' ->
        let* e'' = apply1 eval (s, e') in
        M.ret (VEither (VFalse e''))
    | _ -> M.fail ""
    end
  and eval_fst =
    function (s, e) ->
    begin match e with
    | Fst p ->
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
  and eval_let =
    function (s, e) ->
    begin match e with
    | Let (n, v, b) ->
        let* v' = apply1 eval (s, v) in
        let* s' = apply1 env_write (s, n, v') in
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
        | VEither g' ->
            M.branch [
              (function () ->
                begin match g' with
                | VTrue v ->
                    let* _tmp = apply1 eval (s, on_t) in
                    begin match _tmp with
                    | VFun (n, b, z) ->
                        let* z' = apply1 env_write (z, n, v) in
                        apply1 eval (z', b)
                    | _ -> M.fail ""
                    end
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match g' with
                | VFalse v ->
                    let* _tmp = apply1 eval (s, on_f) in
                    begin match _tmp with
                    | VFun (n, b, z) ->
                        let* z' = apply1 env_write (z, n, v) in
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
  and eval_seq =
    function (s, e) ->
    begin match e with
    | Seq (a, b) ->
        let* _ = apply1 eval (s, a) in
        apply1 eval (s, b)
    | _ -> M.fail ""
    end
  and eval_snd =
    function (s, e) ->
    begin match e with
    | Snd p ->
        let* _tmp = apply1 eval (s, p) in
        begin match _tmp with
        | VPair (_, snd) -> M.ret snd
        | _ -> M.fail ""
        end
    | _ -> M.fail ""
    end
  and eval_true =
    function (s, e) ->
    begin match e with
    | True e' ->
        let* e'' = apply1 eval (s, e') in
        M.ret (VEither (VTrue e''))
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
    | Var n -> apply1 env_read (s, n)
    | _ -> M.fail ""
    end
end