open Interpreter.Interpreter

let evaluate (act_expr: expr_act_t): expr_ch_t =
  let ch_name = make_name "__root" in
  let ch_expr = translate (act_expr, ch_name) |> M.extract in
  (ChLet (ch_name, ChNewCh, ch_expr))
