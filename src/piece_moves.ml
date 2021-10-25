
(*Board starts from 1 index?*)
let two_white_pawn pos pos_lst=

  if fst pos=2 then 
    match Board.piece_of_position (fst pos + 2,snd pos) pos_lst with
    |Piece.Nothing -> [fst pos + 2,snd pos]
    |_ -> []
else []

let one_white_pawn pos pos_lst=
  match Board.piece_of_position (fst pos + 1,snd pos) pos_lst with
    |Piece.Nothing -> [fst pos + 1,snd pos]
    |_ -> []

    
let right_left_white_pawn pos pos_lst num_cols=
let left=
  if snd pos<2 then [] else 
  match Board.piece_of_position (fst pos + 1,snd pos - 1) pos_lst with
    |Piece.Nothing -> []
    |piece -> if Piece.team_of piece = "white" then [] else [(fst pos + 1, snd pos - 1)]
  in 
  let right =
    if snd pos = num_cols then [] else 
      match Board.piece_of_position (fst pos + 1,snd pos + 1) pos_lst with
        |Piece.Nothing -> []
        |piece -> if Piece.team_of piece = "white" then [] else [(fst pos + 1, snd pos + 1)]
    in
        left @ right
  

let white_pawn_det pos pos_lst num_cols=
    (two_white_pawn pos pos_lst) @ (one_white_pawn pos pos_lst) @ (right_left_white_pawn pos pos_lst num_cols)


let white_pawn_moves pos board num_cols=
  white_pawn_det pos board num_cols


  let two_black_pawn pos pos_lst num_rows=
  if fst pos=num_rows-1 then 
    match Board.piece_of_position (fst pos - 2,snd pos) pos_lst with
    |Piece.Nothing -> [fst pos - 2,snd pos]
    |_ -> []
else []

let one_black_pawn pos pos_lst=
  match Board.piece_of_position (fst pos - 1,snd pos) pos_lst with
    |Piece.Nothing -> [fst pos - 1,snd pos]
    |_ -> []

    
let right_left_black_pawn pos pos_lst num_cols=
let left=
  if snd pos<2 then [] else 
  match Board.piece_of_position (fst pos - 1,snd pos - 1) pos_lst with
    |Piece.Nothing -> []
    |piece -> if Piece.team_of piece = "white" then [] else [(fst pos - 1, snd pos - 1)]
  in 
  let right =
    if snd pos = num_cols then [] else 
      match Board.piece_of_position (fst pos - 1,snd pos + 1) pos_lst with
        |Piece.Nothing -> []
        |piece -> if Piece.team_of piece = "white" then [] else [(fst pos - 1, snd pos + 1)]
    in
        left @ right
  

let black_pawn_det pos pos_lst num_cols num_rows=
    (two_black_pawn pos pos_lst num_rows) @ (one_black_pawn pos pos_lst) @ (right_left_black_pawn pos pos_lst num_cols)

let black_pawn_moves pos board num_cols num_rows=
    black_pawn_det pos board num_cols num_rows


let determine_piece_possible piece pos pos_lst num_cols num_rows=
match piece with
| Piece.Nothing -> []
| Piece.White_Pawn _ -> white_pawn_moves pos pos_lst num_cols
| Piece.Black_Pawn _ -> black_pawn_moves pos pos_lst num_cols num_rows
| Piece.Rook _ -> white_pawn_moves pos pos_lst num_cols
| Piece.Bishop _ -> white_pawn_moves pos pos_lst num_cols
| Piece.Knight _ -> white_pawn_moves pos pos_lst num_cols
| Piece.King _ -> white_pawn_moves pos pos_lst num_cols
| Piece.Queen _ -> white_pawn_moves pos pos_lst num_cols

let rec determine_possibles_row board row num_rows num_cols=
match row with
|[]->[]
|h::t->determine_piece_possible (Board.piece_of_position (Board.id_from_position h) board) (Board.id_from_position h) (board) (num_cols) (num_rows) :: determine_possibles_row board t num_rows num_cols


let rec determine_possibles board pos_lst acc num_cols num_rows=
match pos_lst with
|[]->acc
|h::t->determine_possibles board t ((determine_possibles_row board h num_rows num_cols) :: acc) num_cols num_rows

let calc_possible_moves board=
  let pos_lst=Board.positions_from_board board in 
  determine_possibles pos_lst pos_lst [] (Board.num_cols board) (Board.num_rows board)