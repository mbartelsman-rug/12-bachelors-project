module I = Interpreter.Default
module T = Impl.Types
module M = Monads.Identity

open M.Infixes

let rec evaluate_all ((env, expr): T.env_t * T.expr_t): (T.env_t * T.expr_t) M.t =
  I.expr_reduce (env, expr)
  >>= (fun (env', expr') ->
    match expr = expr' with
    | true -> (env', expr') |> M.ret
    | false -> evaluate_all (env', expr')
  )
