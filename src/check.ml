let rec horizontal_helper board pos team direction num_cols num_rows =
  let next = Board.add_tuple pos direction in
  let bool = Board.good_move board next team num_cols num_rows in
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

let bishop_moves piece pos board num_cols num_rows =
  let x = horivertical piece board pos num_cols num_rows in
  x (1, 1) @ x (1, -1) @ x (-1, 1) @ x (-1, -1)

let good_move_helper board pos add team num_cols num_rows =
  let x = Board.add_tuple pos add in
  if fst (Board.good_move board x team num_cols num_rows) then [ x ]
  else []

let knight_helper pos board num_cols num_rows team operator =
  let uptwo =
    good_move_helper board pos (-2, operator 0 1) team num_cols num_rows
  in

  let upone =
    good_move_helper board pos (-1, operator 0 2) team num_cols num_rows
  in
  let downone =
    good_move_helper board pos (1, operator 0 2) team num_cols num_rows
  in
  let downtwo =
    good_move_helper board pos (2, operator 0 1) team num_cols num_rows
  in
  uptwo @ upone @ downone @ downtwo

let knight_moves piece pos board num_cols num_rows =
  let x =
    knight_helper pos board num_cols num_rows (Piece.team_of piece)
  in
  x ( + ) @ x ( - )

let cardinal_helper pos board num_cols num_rows team =
  let up = good_move_helper board pos (-1, 0) team num_cols num_rows in
  let down = good_move_helper board pos (1, 0) team num_cols num_rows in
  let left =
    good_move_helper board pos (0, -1) team num_cols num_rows
  in
  let right =
    good_move_helper board pos (0, 1) team num_cols num_rows
  in
  up @ left @ down @ right

let diagonal_helper pos board num_cols num_rows team =
  let upright =
    good_move_helper board pos (-1, 1) team num_cols num_rows
  in
  let upleft =
    good_move_helper board pos (-1, -1) team num_cols num_rows
  in
  let downleft =
    good_move_helper board pos (1, -1) team num_cols num_rows
  in
  let downright =
    good_move_helper board pos (1, 1) team num_cols num_rows
  in
  upright @ upleft @ downleft @ downright

let king_moves piece pos board num_cols num_rows =
  let team = Piece.team_of piece in

  cardinal_helper pos board num_cols num_rows team
  @ diagonal_helper pos board num_cols num_rows team

let queen_moves piece pos board num_cols num_rows =
  rook_moves piece pos board num_cols num_rows
  @ bishop_moves piece pos board num_cols num_rows

let determine_piece_possible piece pos pos_lst num_cols num_rows =
  match piece with
  | Piece.Nothing -> []
  | Piece.White_Pawn _ -> white_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Black_Pawn _ -> black_pawn_moves pos pos_lst num_cols num_rows
  | Piece.Rook _ -> rook_moves piece pos pos_lst num_cols num_rows
  | Piece.Bishop _ -> bishop_moves piece pos pos_lst num_cols num_rows
  | Piece.Knight _ -> knight_moves piece pos pos_lst num_cols num_rows
  | Piece.King _ -> king_moves piece pos pos_lst num_cols num_rows
  | Piece.Queen _ -> queen_moves piece pos pos_lst num_cols num_rows

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

let calc_possible_moves board num_cols num_rows =
  determine_possibles board board [] num_cols num_rows

type spot =
  | Spot of (int * int)
  | Nuttin

let rec find_king_helper row team =
  match row with
  | [] -> Nuttin
  | h :: t -> (
      match Board.piece_direct h with
      | Piece.King pi ->
          if Piece.piece_info_team pi = team then
            Spot (Board.id_from_position h)
          else find_king_helper t team
      | _ -> find_king_helper t team)

let rec find_king board team =
  match board with
  | [] -> failwith "king not found"
  | h :: t -> (
      match find_king_helper h team with
      | Nuttin -> find_king t team
      | Spot h -> h)

let rec check_helper_row row king_pos =
  match row with
  | h :: t ->
      if List.mem king_pos h then true else check_helper_row t king_pos
  | [] -> false

let rec check_helper moves king_pos =
  match moves with
  | [] -> false
  | h :: t ->
      if check_helper_row h king_pos then true
      else check_helper t king_pos

let check board team num_cols num_rows =
  let king_pos = find_king board team in
  let moves = calc_possible_moves board num_cols num_rows in
  check_helper moves king_pos
