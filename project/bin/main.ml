open Channels

let empty_env = Interpreter.make_env ()


let _ = 
  let _ = Interpreter.(
    (Add (
      (Ret (make_int 10)),
      (Neg (
        (Ret (make_int (-5)))
      ))
    ))
  ) in
  let sample = Interpreter.(
    Let (
      (make_name "x"),
      (Ret (make_int 32)),
      (Add (
        (Var (make_name "x")),
        (Ret (make_int 10)))))) in
  
  Interpreter.run_and_print 5 (empty_env, sample)
