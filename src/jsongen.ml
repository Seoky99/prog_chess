(** Helper that kills alex the individual positionss *)
let string_json r c white = 
  
  (*can change to list, pair if needed*)
  let id_as_string = string_of_int r ^ ", " ^ string_of_int c in
  let black_or_white = if white then "white" else "black" in
  
  "{
      'id' : [" ^ id_as_string ^ "],
      'obstacle': 'none',
      'color' : '" ^ black_or_white ^ "'
      },"

(** [create_board n r c max_col] Creates an [n] x [n] chess board json, starting 
at position ([r],[c]) with the template:
  {
    'name': 'nxn', 
    'positions': [
      {
          'id': (rxc),
          'obstacle': 'none or some'
          'color': 'white or black'
      },
      etc.
    ]
    }
  Note: single string markers will be replaced by double quotes. *) 
let rec create_board n r c sqrn = 

  if sqrn = 0 then "" else 

  let is_even = (r + c) mod 2 = 0 in 

  if c = n then string_json r c is_even  ^ create_board n (r+1) 1 (sqrn-1)
  else string_json r c is_even ^ create_board n r (c+1) (sqrn - 1)

(** Obviously prints out the board*)
let print_board n r c = 

  let sqrn = n * n in 
  let positions = create_board n r c sqrn in 

  let final_string =  " { 'name': '" ^ string_of_int n ^ "x" ^ string_of_int n ^ "', 
  'positions': [ " ^ positions ^ " ]}" in



  print_endline (final_string)