open Base

type literal = int

type name = String.t

type expr =
| Unit
| Seq     of ( expr * expr )
(* Lambda expressions *)
| Let     of ( name * expr * expr )
| Ret     of ( expr )
| Fun     of ( name * expr )
| App     of ( expr * expr )
| Var     of ( name )
(* Integer expressions *)
| Lit     of ( literal )
| Neg     of ( expr )
| Add     of ( expr * expr )
(* Product type expressions *)
| Pair    of ( expr * expr )
| Fst     of ( expr )
| Snd     of ( expr )
(* Sum type expressions *)
| True    of ( expr )
| False   of ( expr )
| Match   of ( expr * expr )
(* 
  (* Concurrency primitives *)
  (* Channels *)
  | NewCh
  | Fork    of ( expr )
  | Give    of ( expr * expr )
  | Take    of ( expr )
  (* Actors *)
  | Self
  | Spawn   of ( expr )
  | Send    of ( expr * expr )
  | Receive
*)

type value =
| VUnit
| VInt    of ( int )
| VFun    of ( name * expr )
| VPair   of ( value * value )
| VEither of ( bool * value )
(* 
  (* Concurrency primitives *)
  (* Channels *)
  | Chan    of ( int ) (* TODO: Update according to OCaml's concurrent model *)
  (* Actors *)
  | Pid     of ( int ) (* TODO: Update according to OCaml's concurrent model *)
*)

type env = value Map.M(String).t

(* let _ =
  (Let (
    ("chanClient"),
    (Fun (
      ("stackCh"),
      (Seq (
        (Give (
          (App (
            (Var ("Push")),
            (Lit (5)))),
          (Var ("stackCh")))),
        (Let (
          ("resCh"),
          (NewCh),
          (Seq (
            (Give (
              (App (
                (Var ("Pop")),
                (Var ("resCh")))),
              (Var ("stackCh")))),
            (Take (
              (Var ("resCh")))))))))))),
    (Let (
      ("stackCh"),
      (NewCh),
      (Seq (
        (Fork (
          (App (
            (App (
              (Var ("chanStack")),
              (Var ("stackCh")))),
            (False (Unit)))))),
        (App (
          (Var ("chanClient")),
          (Var ("stackCh"))))))))))
   *)
