let string_json r c black = 
  
  (*can change to list, pair if needed*)
  let id_as_string = string_of_int r ^ ", " ^ string_of_int c in
  let black_or_white = if black then "black" else "white" in
  
  "{
      'id' : (" ^ id_as_string ^ ")
      'obstacle': 'none'
      'color' : " ^ black_or_white ^ "
    },"

(** Creates an N x N chess board json, given that N is odd*)
let rec create_board x r c max_col = 

  if x = 0 then "" else 

  let is_even = x mod 2 = 0 in 

  if c = max_col then string_json r c is_even  ^ create_board (x-1) (r+1) 1 max_col
  else string_json r c is_even ^ create_board (x-1) r (c+1) max_col

let print_board x r c = 

  let sqrtx = x |> float_of_int |> sqrt |> int_of_float in
  let positions = create_board x r c sqrtx in 

  let final_string =  " { 'name': '" ^ string_of_int sqrtx ^ "x" ^ string_of_int sqrtx ^ "', 
  'positions': [ " ^ positions ^ " ]}" in

  print_endline (final_string)