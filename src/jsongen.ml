let position_maker height width r c white is_last = 
  let board_even = width mod 2 = 0 in 
  let id_as_string = string_of_int r ^ ", " ^ string_of_int c in
  let is_white_square = if white then "white" else "black" in
  let is_white_piece = if r = 1 || r = 2 then "black" else "white" in
  let comma_except_last = if is_last then "" else "," in 

  (*Looks ugly you can't pattern match against constants w/ variable expr :p *)
  let piece = 
    if r = 2 then "black_pawn" 
    else if r = height - 1 then "white_pawn"
    else if r = 1 || r = height then 
      let start_pos = 
        if board_even then ((width - 8) / 2) + 1 else (width - 8 - 1) / 2 + 1 in 
      if c = start_pos then is_white_piece ^ "_rook" 
      else if c = start_pos + 1 then is_white_piece ^ "_knight"
      else if c = start_pos + 2 then is_white_piece ^ "_bishop"
      else if c = start_pos + 3 then is_white_piece ^ "_queen" 
      else if c = start_pos + 4 then is_white_piece ^ "_king"
      else if c = start_pos + 5 then is_white_piece ^ "_bishop"
      else if c = start_pos + 6 then is_white_piece ^ "_knight"
      else if c = start_pos + 7 then is_white_piece ^ "_rook"
      else "nothing" 
    else "nothing" in 

  "{ \"id\" : [" ^ id_as_string ^ "], 
  \"obstacle\": \"none\",
   \"color\" : \"" ^ is_white_square ^ "\", 
   \"piece\": \"" ^ piece ^ "\"" ^ "}" ^ comma_except_last

(** [create_board n r c max_col] Creates an [n] x [n] chess board json, starting 
at position ([r],[c]) with the template:
  {
    "name": [r, c], 
    "positions": [
      {
          'id': (rxc),
          'obstacle': 'none or some'
          'color': 'white or black'
      },
      etc.
    ]
    }
  Note: single string markers will be replaced by double quotes. *) 
let rec create_board_helper height width r c sqrn = 

  if sqrn = 0 then "" else 

  let is_even = (r + c) mod 2 = 0 in 
  let is_last = 
    r = height && c = width in 

  if c = width then position_maker height width r c is_even is_last ^ create_board_helper height width (r+1) 1 (sqrn-1)
  else position_maker height width r c is_even is_last ^ create_board_helper height width r (c+1) (sqrn - 1)

(** Obviously prints out the board*)
let create_board height width r c = 

  let num_positions = height * width in 
  let positions = create_board_helper height width r c num_positions in 

  " { \"name\": [" ^ string_of_int height ^ "," ^ string_of_int width ^ "],
   \"positions\": [ " ^ positions ^ " ]}" 