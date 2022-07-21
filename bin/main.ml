let ch_expr = Channels.(
  (Let (
    (make_name "ch"),
    (NewCh),
    (Seq (
      (Give (
        (Var (make_name "ch")),
        (Ret (make_int 10))
      )),
      (Take (
        (Var (make_name "ch"))
      ))
    ))
  ))
)
    
let ch_string = Channels.(serialize ch_expr)
let act_string = Ch2Act.(translate ch_string)

let ch_res = 
  print_endline "EXECUTING CH -----" ;
  Channels.(execute ch_string) 
let act_res =
  print_endline "\nEXECUTING ACT ----" ;
  Actors.(execute act_string) 

let _ =
  print_endline "\nRESULTS CH -------" ;
  print_endline ch_res ;
  print_endline "\nRESULTS ACT ------" ;
  print_endline act_res
