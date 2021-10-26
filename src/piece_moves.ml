let add_tuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let is_bounds id num_cols num_rows =
  if fst id > num_rows then false
  else if fst id < 1 then false
  else if snd id > num_cols then false
  else if snd id < 1 then false
  else true

let good_move board pos team num_cols num_rows =
  if is_bounds pos num_cols num_rows then
    match Board.piece_of_position pos board with
    | Piece.Nothing -> (true, false)
    | x ->
        if Piece.team_of x = team then (false, false) else (true, true)
  else (false, false)

let rec horizontal_helper board pos team direction num_cols num_rows =
  let next = add_tuple pos direction in
  let bool = good_move board next team num_cols num_rows in
  if fst bool && snd bool then [ next ]
  else if fst bool = true && snd bool = false then
    next
    :: horizontal_helper board next team direction num_cols num_rows
  else []

let horivertical piece board pos num_cols num_rows direction =
  let team = Piece.team_of piece in
  horizontal_helper board pos team direction num_cols num_rows

let two_white_pawn pos pos_lst num_rows =
  if fst pos = num_rows - 1 then
    match Board.piece_of_position (fst pos - 1, snd pos) pos_lst with
    | Piece.Nothing -> (
        match
          Board.piece_of_position (fst pos - 2, snd pos) pos_lst
        with
        | Piece.Nothing -> [ (fst pos - 2, snd pos) ]
        | _ -> [])
    | _ -> []
  else []

let one_white_pawn pos pos_lst =
  if fst pos <= 0 then []
  else
    match Board.piece_of_position (fst pos - 1, snd pos) pos_lst with
    | Piece.Nothing -> [ (fst pos - 1, snd pos) ]
    | _ -> []

let right_left_white_pawn pos pos_lst num_cols =
  let left =
    if snd pos = 1 then []
    else
      match
        Board.piece_of_position (fst pos - 1, snd pos - 1) pos_lst
      with
      | Piece.Nothing -> []
      | piece ->
          if Piece.team_of piece = "white" then []
          else [ (fst pos - 1, snd pos - 1) ]
  in
  let right =
    if snd pos = num_cols then []
    else
      match
        Board.piece_of_position (fst pos - 1, snd pos + 1) pos_lst
      with
      | Piece.Nothing -> []
      | piece ->
          if Piece.team_of piece = "white" then []
          else [ (fst pos - 1, snd pos + 1) ]
  in
  left @ right

let white_pawn_det pos pos_lst num_cols num_rows =
  two_white_pawn pos pos_lst num_rows
  @ one_white_pawn pos pos_lst
  @ right_left_white_pawn pos pos_lst num_cols

let white_pawn_moves pos board num_cols num_rows =
  white_pawn_det pos board num_cols num_rows

let two_black_pawn pos pos_lst =
  if fst pos = 2 then
    match Board.piece_of_position (fst pos + 1, snd pos) pos_lst with
    | Nothing -> (
        match
          Board.piece_of_position (fst pos + 2, snd pos) pos_lst
        with
        | Piece.Nothing -> [ (fst pos + 2, snd pos) ]
        | _ -> [])
    | _ -> []
  else []

let one_black_pawn pos pos_lst num_rows =
  if fst pos = num_rows then []
  else
    match Board.piece_of_position (fst pos + 1, snd pos) pos_lst with
    | Piece.Nothing -> [ (fst pos + 1, snd pos) ]
    | _ -> []

let right_left_black_pawn pos pos_lst num_cols =
  let left =
    if snd pos < 2 then []
    else
      match
        Board.piece_of_position (fst pos + 1, snd pos - 1) pos_lst
      with
      | Piece.Nothing -> []
      | piece ->
          if Piece.team_of piece = "white" then []
          else [ (fst pos + 1, snd pos - 1) ]
  in
  let right =
    if snd pos = num_cols then []
    else
      match
        Board.piece_of_position (fst pos + 1, snd pos + 1) pos_lst
      with
      | Piece.Nothing -> []
      | piece ->
          if Piece.team_of piece = "white" then []
          else [ (fst pos + 1, snd pos + 1) ]
  in
  left @ right

let black_pawn_det pos pos_lst num_cols num_rows =
  two_black_pawn pos pos_lst
  @ one_black_pawn pos pos_lst num_rows
  @ right_left_black_pawn pos pos_lst num_cols

let black_pawn_moves pos board num_cols num_rows =
  black_pawn_det pos board num_cols num_rows

let rook_moves piece pos board num_cols num_rows =
  let x = horivertical piece board pos num_cols num_rows in
  x (0, 1) @ x (0, -1) @ x (1, 0) @ x (-1, 0)

let determine_piece_possible piece pos pos_lst num_cols num_rows =
  match piece with
  | Piece.Nothing -> []
  | Piece.White_Pawn _ -> white_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Black_Pawn _ -> black_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Rook _ -> rook_moves piece pos pos_lst num_cols num_rows
  | Piece.Bishop _ -> white_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Knight _ -> white_pawn_moves pos pos_lst num_cols num_rows
  | Piece.King _ -> white_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Queen _ -> white_pawn_moves pos pos_lst num_cols num_rows

let rec determine_possibles_row board row num_rows num_cols =
  match row with
  | [] -> []
  | h :: t ->
      determine_piece_possible
        (Board.piece_of_position (Board.id_from_position h) board)
        (Board.id_from_position h)
        board num_cols num_rows
      :: determine_possibles_row board t num_rows num_cols

let rec determine_possibles board pos_lst acc num_cols num_rows =
  match pos_lst with
  | [] -> acc
  | h :: t ->
      determine_possibles board t
        (determine_possibles_row board h num_rows num_cols :: acc)
        num_cols num_rows

let calc_possible_moves board =
  let pos_lst = Board.positions_from_board board in
  determine_possibles pos_lst pos_lst [] (Board.num_cols board)
    (Board.num_rows board)
