open Interpreter.Interpreter

let evaluate (expr: expr_ch_t): expr_act_t =
  translate expr
  |> M.extract
