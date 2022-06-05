open Channels

let empty_env = Impl.env_new ()

let rec run_and_print count data =
  let str = Impl.string_of_expr (snd data) in
  print_endline str ;
  ( match count with
    | 0 -> ()
    | _ -> 
      let count' = count - 1 in
      let res = reduce data |> Impl.M.extract in
      run_and_print count' res
  )

let _ = 
  let _ = Impl.(
    (Add (
      (Ret (IntVal 10)),
      (Neg (
        (Ret (IntVal (-5)))
      ))
    ))
  ) in
  let sample = Impl.(
    Let (
      ("x"),
      (Ret (IntVal 32)),
      (Add (
        (Var "x"),
        (Ret (IntVal 10)))))) in
  
  run_and_print 5 (empty_env, sample)
