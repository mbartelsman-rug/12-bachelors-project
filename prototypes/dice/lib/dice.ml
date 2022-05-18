(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  type ident
  type lit
  type state
  type value
  val value_of_lit : lit -> value
  val string_of_value : value -> Base.String.t

  val neg_values : value -> value
  val add_values : value -> value -> value
  val pool_count : state -> value
  val pool_clear : state -> state
end

module Types : TYPES = struct
  open Base
  
  type ident = String.t
  type state = (int Map.M(String).t) * (int List.t)
  type lit = int
  type value = int

  let value_of_lit l = l
  let string_of_value v = Printf.sprintf "%d" v
  let neg_values a = -a
  let add_values a b = a + b
  let pool_count (_, pool) = List.length pool
  let pool_clear (s, _) = (s, [])
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

module IdMonad: MONAD = struct
	exception Branch_fail of string

	type 'a t = 'a

	let ret x = x

	let rec branch l =
		begin match l with
		| [] -> raise (Branch_fail "No branch matches")
		| b1 :: bq ->
				try b1 () with Branch_fail _ -> branch bq
		end

	let fail s = raise (Branch_fail s)

	let bind x f = f x

  let apply f x = f x

  let extract x = x
end


(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: MONAD
  include TYPES

  type stmt =
  | Set of (ident * expr)
  | Seq of (stmt * stmt)
  | Roll of (expr * expr)
  | Print of expr
  | PTop of expr
  | PNeg
  | PGt of expr
  | PAdd of expr
  | Clear
  and expr =
  | Total
  | Neg of expr
  | Get of ident
  | Count
  | Const of lit
  | Add of (expr * expr)

  val add: value * value -> value M.t
  val clear: state -> state M.t
  val count: state -> value M.t
  val litToVal: lit -> value M.t
  val neg: value -> value M.t
  val poolAdd: state * value -> state M.t
  val poolGt: state * value -> state M.t
  val poolNeg: state -> state M.t
  val poolTop: state * value -> state M.t
  val print: value -> unit M.t
  val read: state * ident -> value M.t
  val roll: state * value * value -> state M.t
  val total: state -> value M.t
  val write: state * ident * value -> state M.t
end

(** A default instantiation *)
module Unspec : UNSPEC = struct
  exception NotImplemented of string
  include Types 
  module M = IdMonad

  type stmt =
  | Set of (ident * expr)
  | Seq of (stmt * stmt)
  | Roll of (expr * expr)
  | Print of expr
  | PTop of expr
  | PNeg
  | PGt of expr
  | PAdd of expr
  | Clear
  and expr =
  | Total
  | Neg of expr
  | Get of ident
  | Count
  | Const of lit
  | Add of (expr * expr)

  let add (a, b) = add_values a b |> M.ret
  let clear s = pool_clear s |> M.ret
  let count s = pool_count s |> M.ret
  let litToVal l = value_of_lit l |> M.ret
  let neg a = neg_values a |> M.ret
  let poolAdd _ = raise (NotImplemented "poolAdd")
  let poolGt _ = raise (NotImplemented "poolGt")
  let poolNeg _ = raise (NotImplemented "poolNeg")
  let poolTop _ = raise (NotImplemented "poolTop")
  let print _ = raise (NotImplemented "print")
  let read _ = raise (NotImplemented "read")
  let roll _ = raise (NotImplemented "roll")
  let total _ = raise (NotImplemented "total")
  let write _ = raise (NotImplemented "write")
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type ident
  type lit
  type state
  type value

  type stmt =
  | Set of (ident * expr)
  | Seq of (stmt * stmt)
  | Roll of (expr * expr)
  | Print of expr
  | PTop of expr
  | PNeg
  | PGt of expr
  | PAdd of expr
  | Clear
  and expr =
  | Total
  | Neg of expr
  | Get of ident
  | Count
  | Const of lit
  | Add of (expr * expr)

  val add: value * value -> value M.t
  val clear: state -> state M.t
  val count: state -> value M.t
  val eval_expr: state -> (expr -> value M.t) M.t
  val eval_stmt: state -> (stmt -> state M.t) M.t
  val litToVal: lit -> value M.t
  val neg: value -> value M.t
  val poolAdd: state * value -> state M.t
  val poolGt: state * value -> state M.t
  val poolNeg: state -> state M.t
  val poolTop: state * value -> state M.t
  val print: value -> unit M.t
  val read: state * ident -> value M.t
  val roll: state * value * value -> state M.t
  val total: state -> value M.t
  val write: state * ident * value -> state M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec eval_expr s =
    M.ret (function e ->
      M.branch [
        (function () ->
          begin match e with
          | Total -> apply1 total s
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Count -> apply1 count s
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Const l -> apply1 litToVal l
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Get i -> apply1 read (s, i)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Neg a ->
              let* a' = apply2 eval_expr s a in
              apply1 neg a'
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match e with
          | Add (a, b) ->
              let* a' = apply2 eval_expr s a in
              let* b' = apply2 eval_expr s b in
              apply1 add (a', b')
          | _ -> M.fail ""
          end)])
  and eval_stmt s =
    M.ret (function t ->
      M.branch [
        (function () ->
          begin match t with
          | Seq (a, b) ->
              let* s' = apply2 eval_stmt s a in
              apply2 eval_stmt s' b
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Set (i, e) ->
              let* e' = apply2 eval_expr s e in
              apply1 write (s, i, e')
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Roll (n, z) ->
              let* n' = apply2 eval_expr s n in
              let* z' = apply2 eval_expr s z in
              apply1 roll (s, n', z')
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | PNeg -> apply1 poolNeg s
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | PAdd e ->
              let* e' = apply2 eval_expr s e in
              apply1 poolAdd (s, e')
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | PGt e ->
              let* e' = apply2 eval_expr s e in
              apply1 poolGt (s, e')
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | PTop e ->
              let* e' = apply2 eval_expr s e in
              apply1 poolTop (s, e')
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Print e ->
              let* e' = apply2 eval_expr s e in
              let* _ = apply1 print e' in
              M.ret s
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match t with
          | Clear -> apply1 clear s
          | _ -> M.fail ""
          end)])
end
