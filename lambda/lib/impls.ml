(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: Monads.MONAD

  type env
  and literal
  and name
  and vint
  and vunion =
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
  val int_of_value: value -> vint M.t
  val neg_int: vint -> value M.t
  val value_of_literal: literal -> value M.t
end

(* Implementation for types *)
module Types = struct
  open Base

  (** Environment storing name-value pairs *)
  type env = value Map.M(String).t
  
  and name = String.t
  
  and literal = int

  and vint = int

  and vunion =
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

  (** Empty map constructor *)
  let env_empty: env = Map.empty (module String)

  (** New literal constructor, takes an int *)
  let make_lit (i: int): literal = i

  (** New name constructor, takes a string *)
  let make_name (s: string): name = s

  (** Returns a pretty-fied string of the given
      value *)
  let rec pretty_print (v: value): String.t =
    match v with
    | VUnit             ->                "()"
    | VUnion (VLeft v)  -> Printf.sprintf "Left (%s)"     (pretty_print v)
    | VUnion (VRight v) -> Printf.sprintf "Right (%s)"    (pretty_print v)
    | VPair (a, b)      -> Printf.sprintf "(%s, %s)"      (pretty_print a) (pretty_print b)
    | VInt i            -> Printf.sprintf "%d"            i
    | VFun (n, _, _)    -> Printf.sprintf "\\%s -> (...)" n

  (** Compares two expressions for equality. Two
      expressions are equal if they have the
      exact same syntax tree *)
  let rec equal_expr (a: expr) (b: expr): bool =
    match a with
    | Lit l -> (
      match b with
      | Lit l' -> l = l'
      | _ -> false
    )
    | Unit -> (
      match b with
      | Unit -> true
      | _ -> false
    )
    | Ret e -> (
      match b with
      | Ret e' -> equal_expr e e'
      | _ -> false
    )
    | Neg e -> (
      match b with
      | Neg e' -> equal_expr e e'
      | _ -> false
    )
    | First e -> (
      match b with
      | First e' -> equal_expr e e'
      | _ -> false
    )
    | Second e -> (
      match b with
      | Second e' -> equal_expr e e'
      | _ -> false
    )
    | Left e -> (
      match b with
      | Left e' -> equal_expr e e'
      | _ -> false
    )
    | Right e -> (
      match b with
      | Right e' -> equal_expr e e'
      | _ -> false
    )
    | Seq (e1, e2) -> (
      match b with
      | Seq (e1', e2') -> (equal_expr e1 e1') && (equal_expr e2 e2')
      | _ -> false
    )
    | App (e1, e2) -> (
      match b with
      | App (e1', e2') -> (equal_expr e1 e1') && (equal_expr e2 e2')
      | _ -> false
    )
    | Add (e1, e2) -> (
      match b with
      | Add (e1', e2') -> (equal_expr e1 e1') && (equal_expr e2 e2')
      | _ -> false
    )
    | Pair (e1, e2) -> (
      match b with
      | Pair (e1', e2') -> (equal_expr e1 e1') && (equal_expr e2 e2')
      | _ -> false
    )
    | Match (e1, e2, e3) -> (
      match b with
      | Match (e1', e2', e3') -> (equal_expr e1 e1') && (equal_expr e2 e2') && (equal_expr e3 e3')
      | _ -> false
    )
    | Var n -> (
      match b with
      | Var n' -> String.equal n n'
      | _ -> false
    )
    | Fun (n, e) ->  (
      match b with
      | Fun (n', e') -> (String.equal n n') && (equal_expr e e')
      | _ -> false
    )
    | Let (n, e1, e2) ->  (
      match b with
      | Let (n', e1', e2') -> (String.equal n n') && (equal_expr e1 e1') && (equal_expr e2 e2')
      | _ -> false
    )

  (** Compares two values for equality. Two
      values are equal if they have the same
      internal structure and primitive values *)
  let rec equal_value (a: value) (b: value): bool =
    match a with
    | VUnit -> (
      match b with
      | VUnit -> true
      | _ -> false
      )
    | VUnion (VLeft a') -> (
      match b with
      | VUnion (VLeft b') -> equal_value a' b'
      | _ -> false
      )
    | VUnion (VRight a') -> (
      match b with
      | VUnion (VRight b') -> equal_value a' b'
      | _ -> false
      )
    | VPair (x1, y1) -> (
      match b with
      | VPair (x2, y2) -> (equal_value x1 x2) && (equal_value y1 y2)
      | _ -> false
      )
    | VInt i -> (
      match b with
      | VInt j -> i = j
      | _ -> false
      )
    | VFun (n1, b1, e1) -> (
      match b with
      | VFun (n2, b2, e2) -> (String.equal n1 n2) && (equal_expr b1 b2) && (Map.equal equal_value e1 e2)
      | _ -> false
      )

end

(** The main implementation for the interpreter *)
module Main = struct
  open Base

  include Types
  module M = Monads.Id

  exception RuntimeError of string
    
  let env_get ((e, n): env * name) =
    match Map.find e n with
    | Some v -> v |> M.ret
    | None -> raise (RuntimeError (Printf.sprintf "variable `%s` not bound" n))

  let env_store ((e, n, v): env * name * value) =
    (Map.set e ~key:n ~data:v) |> M.ret

  let int_of_value (v: value) =
    match v with
    | VInt i -> i |> M.ret
    | _ -> raise (RuntimeError "value is not an int")

  let value_of_literal (l: literal) =
    (VInt l) |> M.ret
  
  let neg_int (i: vint) =
    VInt (-i) |> M.ret

  let add_int ((a, b): vint * vint) =
    VInt (a + b) |> M.ret
end

