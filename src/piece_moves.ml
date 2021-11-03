let move_back board original removed org_pos next_pos =
  let _ = Board.check_move original next_pos org_pos board in
  Board.put_piece_pos_lst next_pos removed board

let rec horizontal_helper
    piece
    board
    original
    pos
    team
    direction
    num_cols
    num_rows =
  let next = Board.add_tuple pos direction in
  let bool = Board.good_move board next team num_cols num_rows in
  if fst bool && snd bool then
    let spot = Board.check_move piece original next board in
    if Check.check board team num_cols num_rows then
      let _ = move_back board piece spot original next in
      []
    else
      let _ = move_back board piece spot original next in
      [ next ]
  else if fst bool = true && snd bool = false then
    let spot = Board.check_move piece pos next board in
    if Check.check board team num_cols num_rows then
      let _ = move_back board piece spot original next in
      []
      @ horizontal_helper piece board original next team direction
          num_cols num_rows
    else
      let _ = move_back board piece spot original next in
      next
      :: horizontal_helper piece board original next team direction
           num_cols num_rows
  else []

let horivertical piece board pos num_cols num_rows direction =
  let team = Piece.team_of piece in
  horizontal_helper piece board pos pos team direction num_cols num_rows

let two_white_pawn piece pos pos_lst num_cols num_rows =
  if fst pos = num_rows - 1 then
    match Board.piece_of_position (fst pos - 1, snd pos) pos_lst with
    | Piece.Nothing -> (
        let next = (fst pos - 2, snd pos) in
        match Board.piece_of_position next pos_lst with
        | Piece.Nothing ->
            let spot = Board.check_move piece pos next pos_lst in
            if Check.check pos_lst "white" num_cols num_rows then
              let _ = move_back pos_lst piece spot pos next in
              []
            else
              let _ = move_back pos_lst piece spot pos next in
              [ next ]
        | _ -> [])
    | _ -> []
  else []

let one_white_pawn piece pos pos_lst num_cols num_rows =
  if fst pos <= 0 then []
  else
    let next = (fst pos - 1, snd pos) in
    match Board.piece_of_position next pos_lst with
    | Piece.Nothing ->
        let spot = Board.check_move piece pos next pos_lst in
        if Check.check pos_lst "white" num_cols num_rows then
          let _ = move_back pos_lst piece spot pos next in
          []
        else
          let _ = move_back pos_lst piece spot pos next in
          [ next ]
    | _ -> []

let right_left_white_pawn piece pos pos_lst num_cols num_rows =
  let left =
    if snd pos = 1 then []
    else
      let next = (fst pos - 1, snd pos - 1) in
      match Board.piece_of_position next pos_lst with
      | Piece.Nothing -> []
      | next_piece ->
          let spot = Board.check_move piece pos next pos_lst in
          if Piece.team_of next_piece = "white" then
            let _ = move_back pos_lst piece spot pos next in
            []
          else if Check.check pos_lst "white" num_cols num_rows then
            let _ = move_back pos_lst piece spot pos next in
            []
          else
            let _ = move_back pos_lst piece spot pos next in
            [ next ]
  in
  let right =
    if snd pos = num_cols then []
    else
      let next = (fst pos - 1, snd pos + 1) in
      match Board.piece_of_position next pos_lst with
      | Piece.Nothing -> []
      | next_piece ->
          let spot = Board.check_move piece pos next pos_lst in
          if Piece.team_of next_piece = "white" then
            let _ = move_back pos_lst piece spot pos next in
            []
          else if Check.check pos_lst "white" num_cols num_rows then
            let _ = move_back pos_lst piece spot pos next in
            []
          else
            let _ = move_back pos_lst piece spot pos next in
            [ next ]
  in
  left @ right

let white_pawn_moves piece pos pos_lst num_cols num_rows =
  two_white_pawn piece pos pos_lst num_cols num_rows
  @ one_white_pawn piece pos pos_lst num_cols num_rows
  @ right_left_white_pawn piece pos pos_lst num_cols num_rows

let two_black_pawn piece pos pos_lst num_cols num_rows =
  if fst pos = 2 then
    match Board.piece_of_position (fst pos + 1, snd pos) pos_lst with
    | Nothing -> (
        let next = (fst pos + 2, snd pos) in
        match Board.piece_of_position next pos_lst with
        | Piece.Nothing ->
            let spot = Board.check_move piece pos next pos_lst in
            if Check.check pos_lst "black" num_cols num_rows then
              let _ = move_back pos_lst piece spot pos next in
              []
            else
              let _ = move_back pos_lst piece spot pos next in
              [ next ]
        | _ -> [])
    | _ -> []
  else []

let one_black_pawn piece pos pos_lst num_cols num_rows =
  if fst pos = num_rows then []
  else
    let next = (fst pos + 1, snd pos) in
    match Board.piece_of_position next pos_lst with
    | Piece.Nothing ->
        let spot = Board.check_move piece pos next pos_lst in
        if Check.check pos_lst "black" num_cols num_rows then
          let _ = move_back pos_lst piece spot pos next in
          []
        else
          let _ = move_back pos_lst piece spot pos next in
          [ next ]
    | _ -> []

let right_left_black_pawn piece pos pos_lst num_cols num_rows =
  let left =
    if snd pos < 2 then []
    else
      let next = (fst pos + 1, snd pos - 1) in
      match Board.piece_of_position next pos_lst with
      | Piece.Nothing -> []
      | the_piece ->
          let spot = Board.check_move piece pos next pos_lst in
          if Piece.team_of the_piece = "black" then
            let _ = move_back pos_lst piece spot pos next in
            []
          else if Check.check pos_lst "black" num_cols num_rows then
            let _ = move_back pos_lst piece spot pos next in
            []
          else
            let _ = move_back pos_lst piece spot pos next in
            [ next ]
  in
  let right =
    if snd pos = num_cols then []
    else
      let next = (fst pos + 1, snd pos + 1) in
      match Board.piece_of_position next pos_lst with
      | Piece.Nothing -> []
      | the_piece ->
          if Piece.team_of the_piece = "black" then []
          else
            let spot = Board.check_move piece pos next pos_lst in
            if Check.check pos_lst "black" num_cols num_rows then
              let _ = move_back pos_lst piece spot pos next in
              []
            else
              let _ = move_back pos_lst piece spot pos next in
              [ next ]
  in
  left @ right

let black_pawn_det piece pos pos_lst num_cols num_rows =
  two_black_pawn piece pos pos_lst num_cols num_rows
  @ one_black_pawn piece pos pos_lst num_cols num_rows
  @ right_left_black_pawn piece pos pos_lst num_cols num_rows

let black_pawn_moves pos board num_cols num_rows =
  black_pawn_det pos board num_cols num_rows

let rook_moves piece pos board num_cols num_rows =
  let x = horivertical piece board pos num_cols num_rows in
  x (0, 1) @ x (0, -1) @ x (1, 0) @ x (-1, 0)

let bishop_moves piece pos board num_cols num_rows =
  let x = horivertical piece board pos num_cols num_rows in
  x (1, 1) @ x (1, -1) @ x (-1, 1) @ x (-1, -1)

let good_move_helper piece board pos add team num_cols num_rows =
  let x = Board.add_tuple pos add in
  if fst (Board.good_move board x team num_cols num_rows) then
    let spot = Board.check_move piece pos x board in
    if Check.check board team num_cols num_rows then
      let _ = move_back board piece spot pos x in
      []
    else
      let _ = move_back board piece spot pos x in
      [ x ]
  else []

let knight_helper piece pos board num_cols num_rows team operator =
  let uptwo =
    good_move_helper piece board pos
      (-2, operator 0 1)
      team num_cols num_rows
  in

  let upone =
    good_move_helper piece board pos
      (-1, operator 0 2)
      team num_cols num_rows
  in
  let downone =
    good_move_helper piece board pos
      (1, operator 0 2)
      team num_cols num_rows
  in
  let downtwo =
    good_move_helper piece board pos
      (2, operator 0 1)
      team num_cols num_rows
  in
  uptwo @ upone @ downone @ downtwo

let knight_moves piece pos board num_cols num_rows =
  let x =
    knight_helper piece pos board num_cols num_rows
      (Piece.team_of piece)
  in
  x ( + ) @ x ( - )

let cardinal_helper piece pos board num_cols num_rows team =
  let up =
    good_move_helper piece board pos (-1, 0) team num_cols num_rows
  in
  let down =
    good_move_helper piece board pos (1, 0) team num_cols num_rows
  in
  let left =
    good_move_helper piece board pos (0, -1) team num_cols num_rows
  in
  let right =
    good_move_helper piece board pos (0, 1) team num_cols num_rows
  in
  up @ left @ down @ right

let diagonal_helper piece pos board num_cols num_rows team =
  let upright =
    good_move_helper piece board pos (-1, 1) team num_cols num_rows
  in
  let upleft =
    good_move_helper piece board pos (-1, -1) team num_cols num_rows
  in
  let downleft =
    good_move_helper piece board pos (1, -1) team num_cols num_rows
  in
  let downright =
    good_move_helper piece board pos (1, 1) team num_cols num_rows
  in
  upright @ upleft @ downleft @ downright

let king_moves piece pos board num_cols num_rows =
  let team = Piece.team_of piece in

  cardinal_helper piece pos board num_cols num_rows team
  @ diagonal_helper piece pos board num_cols num_rows team

let queen_moves piece pos board num_cols num_rows =
  rook_moves piece pos board num_cols num_rows
  @ bishop_moves piece pos board num_cols num_rows

let determine_piece_possible piece pos pos_lst num_cols num_rows =
  match piece with
  | Piece.Nothing -> []
  | Piece.White_Pawn _ ->
      white_pawn_moves piece pos pos_lst num_cols num_rows
  | Piece.Black_Pawn _ ->
      black_pawn_moves piece pos pos_lst num_cols num_rows
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

let team_of_space id board =
  Piece.team_of (Board.piece_of_position id board)

let rec team_moves_helper row board team row_num col_num =
  match row with
  | [] -> []
  | h :: t ->
      if team_of_space (row_num, col_num) board = team then
        h :: team_moves_helper t board team row_num col_num
      else team_moves_helper t board team row_num col_num

let rec team_moves moves board team acc row =
  match moves with
  | [] -> acc
  | h :: t ->
      team_moves t board team
        (team_moves_helper h board team row 1 :: acc)
        1

let any_moves moves board team =
  if team_moves moves board team [] 1 = [] then false else true
