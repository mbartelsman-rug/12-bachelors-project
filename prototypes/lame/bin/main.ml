let sample = Lame.Unspec.(
  (Let (
    (make_name "x"),
    (Lit (make_literal 42)),
    (Add (
      (Var (make_name "x")),
      (Lit (make_literal 6))
    ))
  ))
)

let result = Lame.(LameInterpreter.eval (Unspec.make_env, sample)) |> Lame.Id.extract

let () = print_endline (Lame.Unspec.print_value result)
