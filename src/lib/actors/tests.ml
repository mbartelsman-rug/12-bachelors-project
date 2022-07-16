open Utilities
open Interpreter.Interpreter

let prepare results = results
  |> Base.List.map ~f:(fun (_, res) -> res)

let%test "test_ret" =
  let left = [Ret (make_int 56)] in
  let right = evaluate (Ret (make_int 56))
  |> prepare in
  (left = right)

let%test "test_func" =
  let left = [
    Ret (
      (FuncVal (
        (make_name "x"),
        (Var (make_name "x")))))
  ] in
  let right = evaluate (
    Func (
      (make_name "x"),
      (Var (make_name "x")))
  )
  |> prepare in
  (left = right)

let%test "test_call" =
  let left = [
    Ret (make_int 0)
  ] in
  let right = evaluate (
    Call (
      (Func (
        (make_name "x"),
        (Var (make_name "x"))
      )),
      (Ret (make_int 0))
    )
  )
  |> prepare in
  (left = right)

let%test "test_let" =
  let left = [
    Ret (make_int 0)
  ] in
  let right = evaluate (
    Let (
      (make_name "x"),
      (Ret (make_int 0)),
      (Var (make_name "x"))
    )
  )
  |> prepare in
  (left = right)

let%test "test_neg" =
  let left = [
    Ret (make_int (-5))
  ] in
  let right = evaluate (
    Neg (Ret (make_int 5))
  )
  |> prepare in
  (left = right)

let%test "test_add" =
  let left = [
    Ret (make_int 8)
  ] in
  let right = evaluate (
    Add (
      (Ret (make_int 6)),
      (Ret (make_int 2))
    )
  )
  |> prepare in
  (left = right)

let%test "test_sub" =
  let left = [
    Ret (make_int 4)
  ] in
  let right = evaluate (
    Sub (
      (Ret (make_int 6)),
      (Ret (make_int 2))
    )
  )
  |> prepare in
  (left = right)

let%test "test_mul" =
  let left = [
    Ret (make_int 12)
  ] in
  let right = evaluate (
    Mul (
      (Ret (make_int 6)),
      (Ret (make_int 2))
    )
  )
  |> prepare in
  (left = right)

let%test "test_div" =
  let left = [
    Ret (make_int 3)
  ] in
  let right = evaluate (
    Div (
      (Ret (make_int 6)),
      (Ret (make_int 2))
    )
  )
  |> prepare in
  (left = right)

let%test "test_pair" =
  let left = [
    Ret (
      (PairVal (
        (make_int 1),
        (make_int 2)
      ))
    )
  ] in
  let right = evaluate (
    (Pair (
      (Ret (make_int 1)),
      (Ret (make_int 2))
    ))
  )
  |> prepare in
  (left = right)

let%test "test_fst" =
  let left = [
    Ret (make_int 1)
  ] in
  let right = evaluate (
    Fst (
      (Pair (
        (Ret (make_int 1)),
        (Ret (make_int 2))
      ))
    )
  )
  |> prepare in
  (left = right)

let%test "test_snd" =
  let left = [
    Ret (make_int 2)
  ] in
  let right = evaluate (
    Snd (
      (Pair (
        (Ret (make_int 1)),
        (Ret (make_int 2))
      ))
    )
  )
  |> prepare in
  (left = right)

let%test "test_left" =
  let left = [
    Ret (
      (EitherVal (
        (LeftVal (UnitVal))
      ))
    )
  ] in
  let right = evaluate (
    Left (Ret UnitVal)
  )
  |> prepare in
  (left = right)

let%test "test_right" =
  let left = [
    Ret (
      (EitherVal (
        (RightVal UnitVal)
      ))
    )
  ] in
  let right = evaluate (
    Right (Ret UnitVal)
  )
  |> prepare in
  (left = right)


let%test "test_comms" =
  let (>>) = fun a b -> Seq (a, b) in
  let left = [
    Ret (make_int 7)
  ] in
  let right = evaluate (
    (Send (
      (Ret (make_int 7)),
      (Self)
    )) >>
    (Receive)
  )
  |> prepare in
  (left = right)

let%test "test_spawn" =
  let (>>) = fun a b -> Seq (a, b) in
  let left = [
    Ret (make_int 7) ;
    Ret (make_int 7)
  ] in
  let right = evaluate (
    Spawn (
      (Ret (make_int 7))
    ) >>
    (Ret (make_int 7))
  )
  |> prepare in
  (left = right)

let%test "test_spawned_comms" =
let (>>) = fun a b -> Seq (a, b) in
let left = [
  Ret (make_int 7) ;
  Ret UnitVal ;
] in
let right = evaluate (
  Let (
    (make_name "parent"),
    (Self),
    (Spawn (
      (Send (
        (Ret (make_int 7)),
        (Var "parent")
      ))
    )) >>
    (Receive)
  )
)
|> prepare in
(left = right)
