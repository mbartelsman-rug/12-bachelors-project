open Base
module Monad = Monads.Id
module Eval = Interpreters.Main
module Types = Impls.Types

let%test "test_add" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 42))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Add (
        (Lit (make_lit 21)),
        (Lit (make_lit 21))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_neg" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit (-42)))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Neg (
        (Lit (make_lit 42))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_pair_fst" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 1))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (First (
        (Pair (
          (Lit (make_lit 1)),
          (Lit (make_lit 2))
        ))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)
  
let%test "test_pair_snd" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 2))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Second (
        (Pair (
          (Lit (make_lit 1)),
          (Lit (make_lit 2))
        ))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_union_left" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 1))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Match (
        (Left (Unit)),
        (Fun (
          (make_name "x"),
          (Lit (make_lit 1))
        )),
        (Fun (
          (make_name "x"),
          (Lit (make_lit 2))
        ))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_union_right" =
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 2))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Match (
        (Right (Unit)),
        (Fun (
          (make_name "x"),
          (Lit (make_lit 1))
        )),
        (Fun (
          (make_name "x"),
          (Lit (make_lit 2))
        ))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_let" = 
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 1))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Let (
        (make_name "x"),
        (Lit (make_lit 1)),
        (Var (make_name "x"))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_fun" = 
  let reference =
    Eval.eval Types.(
      env_empty,
      (Fun (
        (make_name "x"),
        (Var (make_name "x"))
      ))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Let (
        (make_name "f"),
        (Fun (
          (make_name "x"),
          (Var (make_name "x"))
        )),
        (Var (make_name "f"))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_app" = 
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 1))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (App (
        (Fun (
          (make_name "x"),
          (Var (make_name "x"))
        )),
        (Lit (make_lit 1))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)

let%test "test_seq" = 
  let reference =
    Eval.eval Types.(
      env_empty,
      (Lit (make_lit 2))
    )
    |> Monad.extract
  in
  let operation =
    Eval.eval Types.(
      env_empty,
      (Seq (
        (Lit (make_lit 1)),
        (Lit (make_lit 2))
      ))
    )
    |> Monad.extract
  in
  (Types.equal_value reference operation)
