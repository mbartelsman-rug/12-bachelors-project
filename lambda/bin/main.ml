open Base
open Lambda

let () =
  let result =
    Eval.eval Types.(
      env_empty,
      (Fun (
        (make_name "x"),
        (Var (make_name "x"))
      ))
    )
    |> Monad.extract
  in
  Stdio.print_endline (Types.pretty_print result)

