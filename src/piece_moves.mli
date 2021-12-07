val white_castle : bool ref
(** true if white can castle *)

val black_castle : bool ref
(** true if black can castle *)

val white_pawn_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(** [white_pawn_moves pos board num_cols] is the list of id of possible
    spaces for a white pawn to move give the position list board and the
    number of columns num_cols *)

val black_pawn_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(** [black_pawn_moves pos pos_list num_cols num_rows] is the list of
    (int*int) locations that a black pawn on the given board can move
    to.*)

val rook_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(** [rook_moves piece pos board num_cols num_rows] returns the possible
    positions that the [piece] (which is a rook) can move at an [id] in
    some [board]*)

val determine_piece_possible :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(**[determine_piece_possible piece pos pos_lst num_cols num_rows] is the
   (int*int)list of possible moves of the piece at that space. If there
   are are none, then the method evaluates to []*)

val calc_possible_moves :
  Board.position list list -> int -> int -> (int * int) list list list
(** [calc_possible_moves board] is a 2D list of possible moves for each
    space on the board, where each space is either [] or a list of
    (int*int) tuples that dictate that space's possible moves. List
    starts from position (8,1) and ends at (1,8)*)

val bishop_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list

val knight_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list

val king_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list

val queen_moves :
  Piece.piece ->
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list

val team_moves :
  'a list list ->
  Board.position list list ->
  string ->
  'a list list ->
  int ->
  'a list list
(** [team_moves moves board team acc row] is the list of moves for a
    team *)

val any_moves :
  'a list list -> Board.position list list -> string -> bool
(** [any_moves moves board team] is true if a team has moves and false
    if not*)
