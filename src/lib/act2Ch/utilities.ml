open Interpreter.Interpreter

let evaluate (act_expr: expr_act_t): expr_ch_t =
  let ch_name = make_name "__root" in
  let ch_expr = translate (act_expr, ch_name) |> M.extract in
  (Let (ch_name, NewCh, ch_expr))

let parse (strn: string): expr_act_t =
  expr_act_of_string strn

let serialize (expr: expr_ch_t): string =
  string_of_expr_ch expr

let run (program: string): string =
  parse program
  |> evaluate
  |> serialize
