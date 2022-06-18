let rec print_list list =
  match list with
  | [] -> ()
  | hd :: tail ->
  ( Stdio.print_endline (Ch2Act.string_of_act_expr hd) ;
    print_list tail
  )

let _ = 
  let a: Ch2Act.expr_ch_t = Ch2Act.(
    (Add (
      (Ret (make_int 10)),
      (Neg (
        (Ret (make_int (-5)))
      ))
    ))
  ) in
  let b: Ch2Act.expr_ch_t = Ch2Act.(
    Let (
      (make_name "x"),
      (Ret (make_int 32)),
      (Add (
        (Var (make_name "x")),
        (Ret (make_int 10)))))) in
  let c: Ch2Act.expr_ch_t = Ch2Act.(
    Give (
      (Var (make_name "ch")),
      (Ret (make_int 100))
    )
  ) in
  let d: Ch2Act.expr_ch_t = Ch2Act.(
    Take (
      (Var (make_name "ch"))
    )
  ) in

  let sample: Ch2Act.expr_ch_t = Ch2Act.(
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

  let _ = Actors.(
    Right (Ret UnitVal)
  ) in
  
  (* print_list (Actors.evaluate sample ~print:true) *)
  print_list [Ch2Act.evaluate sample]
