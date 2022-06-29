let ch_expr = Channels.(
  let a = 
    (Add (
      (Ret (make_int 10)),
      (Neg (
        (Ret (make_int (-5))))))) in

  let b = 
    Let (
      (make_name "x"),
      (Ret (make_int 32)),
      (Add (
        (Var (make_name "x")),
        (Ret (make_int 10))))) in

  let c = 
    Give (
      (Var (make_name "ch")),
      (Ret (make_int 100))) in

  let d = 
    Take (
      (Var (make_name "ch"))) in
  
  Let (
    (make_name "ch"),
    (NewCh),
    (Seq (
      (Fork a),
      (Seq (
        (Fork b),
        (Seq (
          (Fork c),
          (Fork d)))))))))
    
let ch_string = Channels.(serialize ch_expr)
let act_string = Ch2Act.(translate ch_string)

(* let _ =
  print_endline "CH -----------" ;
  print_endline ch_string ;
  print_endline "ACT ----------" ;
  print_endline act_string *)

let ch_res = Channels.(execute ch_string) 
let act_res = Actors.(execute act_string) 

let _ =
  print_endline "CH RES -------" ;
  print_endline ch_res ;
  print_endline "ACT RES ------" ;
  print_endline act_res
