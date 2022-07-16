open Interpreter.Interpreter

let evaluate (expr: expr_ch_t): expr_act_t =
  translate expr
  |> M.extract

let parse (strn: string): expr_ch_t =
  expr_ch_of_string strn

let serialize (expr: expr_act_t): string =
  string_of_expr_act expr

let run (program: string): string =
  parse program
  |> evaluate
  |> serialize
