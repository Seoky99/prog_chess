val white_pawn_moves :
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(** [white_pawn_moves pos board num_cols] is the list of id of possible
    spaces for a white pawn to move give the position list board and the
    number of columns num_cols *)

val black_pawn_moves :
  int * int ->
  Board.position list list ->
  int ->
  int ->
  (int * int) list
(** [black_pawn_moves pos pos_list num_cols num_rows] is the list of
    (int*int) locations that a black pawn on the given board can move
    to.*)

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

val calc_possible_moves : Board.board -> (int * int) list list list
(** [calc_possible_moves board] is a 2D list of possible moves for each
    space on the board, where each space is either [] or a list of
    (int*int) tuples that dictate that space's possible moves. *)
