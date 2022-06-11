open Channels

let rec print_list list =
  match list with
  | [] -> ()
  | hd :: tail ->
  ( Stdio.print_endline (Interpreter.string_of_expr hd) ;
    print_list tail
  )

let _ = 
  let a = Interpreter.(
    (Add (
      (Ret (make_int 10)),
      (Neg (
        (Ret (make_int (-5)))
      ))
    ))
  ) in
  let b = Interpreter.(
    Let (
      (make_name "x"),
      (Ret (make_int 32)),
      (Add (
        (Var (make_name "x")),
        (Ret (make_int 10)))))) in
  let c = Interpreter.(
    Give (
      (Var (make_name "ch")),
      (Ret (make_int 100))
    )
  ) in
  let d = Interpreter.(
    Take (
      (Var (make_name "ch"))
    )
  ) in

  let sample = Interpreter.(
    Let (
      (make_name "ch"),
      (NewCh),
      (Seq (
        (Fork a),
        (Seq (
          (Fork b),
          (Seq (
            (Fork c),
            (Fork d))))))))
  ) in
  
  print_list (Interpreter.evaluate sample ~print:true)
