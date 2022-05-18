open Base

let max_widths table =
  let row_lens row = List.map row ~f:(String.length) in
  let row_max r1 r2 = List.map2_exn r1 r2 ~f:(max) in

  let widths = List.map table ~f:(row_lens) in
  let max_widths_maybe = List.reduce widths ~f:(row_max) in
  
  match max_widths_maybe with
  | Some mw -> mw
  | None -> []

let pad str size = String.concat [str ; String.make (size - String.length str) ' ']

let print_table table = 
  let row_widths = max_widths table in

  let pad_row row = List.map2_exn row row_widths ~f:(pad) in
  let row_core row = String.concat ~sep:" | " (pad_row row) in
  let row_final row = String.concat ["| " ; row_core row ; " |"] in
  let divider = List.map row_widths ~f:(fun width -> String.make width '-') in

  let rowed = List.map table ~f:(row_final) in
  let final_table = match rowed with
  | [] -> []
  | head :: body -> head :: (row_final divider) :: body in

  List.iter final_table ~f:(fun row -> Stdio.print_endline row)

let () = print_table [["Language" ; "Sentence"] ; ["English" ; "Hello, world!"] ; ["Spanish" ; "!Hola, mundo!"]]
