let rec print_list list =
  match list with
  | [] -> ()
  | hd :: tail ->
  ( Stdio.print_endline (Channels.string_of_expr hd) ;
    print_list tail
  )

let _ = 
  let a = Channels.(
    (Add (
      (Ret (make_int 10)),
      (Neg (
        (Ret (make_int (-5)))
      ))
    ))
  ) in
  let b = Channels.(
    Let (
      (make_name "x"),
      (Ret (make_int 32)),
      (Add (
        (Var (make_name "x")),
        (Ret (make_int 10)))))) in
  let c = Channels.(
    Give (
      (Var (make_name "ch")),
      (Ret (make_int 100))
    )
  ) in
  let d = Channels.(
    Take (
      (Var (make_name "ch"))
    )
  ) in

  let _ = Channels.(
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

  let (>>) = fun a b -> Channels.Seq (a, b) in
  let sample = Channels.(
    Let (
      (make_name "ch"),
      (NewCh),
      (Fork (
        (Take (
          (Var (make_name "ch"))
        ))
      )) >>
      (Fork (
        (Give (
          (Var (make_name "ch")),
          (Ret (make_int 7))
        ))
      ))
    )
  ) in
  
  print_list (Channels.evaluate sample ~print:true)
