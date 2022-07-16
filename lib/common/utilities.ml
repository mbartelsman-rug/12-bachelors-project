let string_of_sexp sexp = 
  let open Base in
  let buff = Buffer.create 0 in
  let formatter = Caml.Format.formatter_of_buffer buff in
  Sexp.pp_hum formatter sexp;
  Caml.Format.pp_print_flush formatter ();
  Buffer.contents buff
