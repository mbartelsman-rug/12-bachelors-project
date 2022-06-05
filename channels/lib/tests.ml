open Evaluator

let empty_env = Impl.Spec.env_new ()

let%test "test_let_add" = 
  let reference = evaluate_all Impl.Types.(
    empty_env,
    (Ret (IntVal 42))
  ) in
  let operation = evaluate_all Impl.Types.(
    empty_env,
    (Let (
      ("x"),
      (Ret (IntVal 32)),
      (Add (
        (Var "x"),
        (Ret (IntVal 10))
      ))
    ))
  ) in
  (reference = operation)

