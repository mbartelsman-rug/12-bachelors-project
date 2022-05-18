open Monad
open Types

(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: MONAD
  include TYPES

  type monop =
  | Neg
  and expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Add

  val litToVal: literal -> value M.t

  val add: value -> (value -> value M.t) M.t

  val sub: value -> (value -> value M.t) M.t
end

(** A default instantiation *)
module Unspec: UNSPEC = struct
  module M = Monad
  include Types

  type monop =
  | Neg
  and expr =
  | Const of literal
  | Binop of (binop * expr * expr)
  and binop =
  | Sub
  | Add

  let litToVal (n: literal) = match n with
    | Literal n -> M.ret (Value n)

  let add (Value lhs) = M.ret (fun ((Value rhs)) -> M.ret (Value (lhs + rhs)))

  let sub (Value lhs) = M.ret (fun ((Value rhs)) -> M.ret (Value (lhs - rhs)))
end
