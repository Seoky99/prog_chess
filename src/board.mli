type board
(** Abstract type of values representing a chess board. *)

type position
(**Abstract type of values representing a chess square*)

type id = int * int
(** Where the chess square location is*)

val board_from_json : Yojson.Basic.t -> board
(** [board_from_json json] is the chess board that [json]
    represents.contents Requires: [json] is a rectangular
    representation.*)

val n_row : int -> board -> position list
(** [n_row n board] is the nth row of the chess board Requires: [n] >= 1*)

val n_col : int -> board -> position list
(** [n_row n board] is the nth column of the chess board Requires: [n]
    >= 1*)

val id_pos_lst : position list -> id list
(** [id_lst id_pos_lst] produces a list of ids from a list of positions *)

val id_board : board -> id list list
(** [id_board board] produces a list of id lists from a board,
    essentially mapping out the whole board *)

(* [piece_of_position pos lst] is the piece at the position pos in a
   position list lst*)
val piece_of_position : int * int -> position list list -> Piece.piece

val num_rows : board -> int
(** [num_rows board] returns the number of rows from a board*)

val num_cols : board -> int
(** [num_cols board] returns the number of columns from a board*)

val get_color : board -> id -> string
(** [get_color board id] the ouput is the color of the board at that
    board with the given id Requires: id is a valid location on the
    board*)

val position_board : board -> position list list
(** [position_board board] is the 2D position list that represents the
    game state*)

val positions_from_board : board -> position list list
(** [positions_from_board board] is the position list list of board *)

val get_obstacle : board -> id -> string
(** [get_obstacle board id ] the output is the obstacle of the board at
    the specified id Requires: id is a valid location on the board*)

val piece_pos_lst : position list -> string list
(** [piece_pos_lst pos_lst] produces a list of names of the pieces from
    the position lists.*)

val piece_board : board -> string list list
(** [piece_board pos_lst] produces a list of names of the pieces on the
    board.*)

val id_from_position : position -> id
(** [id_from_position] of position is the id of that position. *)

val put_piece : id -> Piece.piece -> board -> board(** [put_piece id piece board] puts the [piece] on the [board] at [id].
    Raises: Exception when there is already a piece there *)
