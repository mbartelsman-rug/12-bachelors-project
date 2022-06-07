module Interpreter: sig
  include Interpreter.TYPES
  module M: Common.Monads.MONAD

  val evaluate_all: (env_t * expr_t) -> (env_t * expr_t) M.t
  val evaluate: (env_t * expr_t) -> expr_t
  val reduce: (env_t * expr_t) -> (env_t * expr_t)
  val run_and_print: int -> (env_t * expr_t) -> unit

end = struct
  include Interpreter.Interpreter

  let rec evaluate_all (env, expr) =
    let* (env', expr') = expr_reduce (env, expr) in
    match expr = expr' with
    | true -> (env', expr') |> M.ret
    | false -> evaluate_all (env', expr')
  
  let evaluate = fun (x) -> snd (M.extract (evaluate_all x))

  let reduce = fun (x) -> M.extract (expr_reduce x)
  
  let rec run_and_print count data =
    let str = string_of_expr (snd data) in
    print_endline str ;
    ( match count with
      | 0 -> ()
      | _ -> 
        let count' = count - 1 in
        let res = reduce data in
        run_and_print count' res
    )
end
