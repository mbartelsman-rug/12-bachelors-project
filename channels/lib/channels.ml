module Impl = Impl.Spec
let evaluate = fun (x) -> snd (Impl.M.extract (Evaluator.evaluate_all x))
let reduce = Interpreter.Default.expr_reduce
