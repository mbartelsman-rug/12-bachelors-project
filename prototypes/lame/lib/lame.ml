(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

include Monads

(** The unspecified types *)
module type TYPES = sig
  type env
  type literal
  type name
  type v_int
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
module Unspec = struct
  open Base

  exception NotImplemented of string
  exception RuntimeError of string
  module M = Monads.Id

  type env = (name, value, String.comparator_witness) Map.t

  and literal = Int.t

  and name = String.t

  and value =
  | VUnit
  | VPair of (value * value)
  | VInt of v_int
  | VFun of (name * expr * env)
  | VEither of v_either

  and v_int = Int.t

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

  let make_name (s: String.t): name = s
  let make_literal (i: Int.t): literal = i
  let make_env: env = Map.empty (module String)

  let rec print_value (v: value): String.t = 
    match v with
    | VUnit -> "()"
    | VPair (a, b) -> Printf.sprintf "(%s, %s)" (print_value a) (print_value b)
    | VInt i -> Printf.sprintf "%d" i
    | VFun (n, _, _) -> Printf.sprintf "\\%s -> (...)" n
    | VEither (VTrue v') -> Printf.sprintf "True (%s)" (print_value v')
    | VEither (VFalse v') -> Printf.sprintf "False (%s)" (print_value v')

  let add_int (a, b) =
    VInt (a + b)
    |> M.ret

  let env_read ((e, n): env * name) : value M.t =
    (
      match Map.find e n with
      | Some (VFun _ as v)
      | Some (VInt _ as v) -> v
      | Some (_) -> raise (RuntimeError "not a function or integer")
      | None -> raise (RuntimeError "name not bound")
    )
    |> M.ret

  let env_write ((e, n, v): (env * name * value)) =
    (
      match Map.add e ~key:n ~data:v with
      | `Ok m -> m
      | `Duplicate ->
        let e' = Map.remove e n in
        (Map.add_exn e' ~key:n ~data:v)
    )
    |> M.ret
  
  let int_of_value v =
    (
      match v with
      | VInt i -> i
      | _ -> raise (RuntimeError "not an integer")
    )
    |> M.ret

  let neg_int a =
    VInt (-a)
    |> M.ret

  let value_of_literal l =
    VInt l
    |> M.ret

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

module LameInterpreter = MakeInterpreter (Unspec)
