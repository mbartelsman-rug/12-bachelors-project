open Arith.Interpreter.Interpreter

let () = eval (Binop ((Add), (Const (Literal 30)), (Const (Literal 12))))
  |> M.extract
  |> fun (Value n) -> Printf.sprintf "Result: %d" n
  |> print_endline
