type board
(** Abstract type of values representing a chess board. *)

type position
(**Abstract type of values representing a chess square*)

type id = int * int
(** Where the chess square location is*)

val board_from_json : Yojson.Basic.t -> board
(** [board_from_json json] directly creates a board from a file that
    contains [json]. Note: if you are looking to create rectangular
    boards, use create_board. This is intended for wacky shapes.*)

val create_board : int -> int -> board
(** [create_board height width] is the chess board of [height] *
    [width]. Requires: Board is a rectangular representation.*)

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
val piece_of_position : id -> position list list -> Piece.piece

val num_rows : board -> int
(** [num_rows board] returns the number of rows from a board*)

val num_cols : board -> int
(** [num_cols board] returns the number of columns from a board*)

val get_color : board -> id -> string
(** [get_color board id] the ouput is the color of the board at that
    board with the given id Requires: id is a valid location on the
    board*)

val get_obstacle : board -> id -> string
(** [get_obstacle board id ] the output is the obstacle of the board at
    the specified id Requires: id is a valid location on the board*)

val piece_pos_lst : position list -> string list
(** [piece_pos_lst pos_lst] produces a list of names of the pieces from
    the position lists.*)

val positions_from_board : board -> position list list
(** [positions_from_board board] is the position list list of board *)

val piece_board : board -> string list list
(** [piece_board pos_lst] produces a list of names of the pieces on the
    board.*)

val id_from_position : position -> id
(** [id_from_position] of position is the id of that position. *)

val position_from_id : id -> board -> position
(** [position_from_id id board] returns the [position] at [id] of
    [board]*)

val put_piece : id -> Piece.piece -> board -> unit
(** [put_piece id piece board] puts the [piece] on the [board] at [id]. *)

val remove_piece : id -> board -> unit
(** [remove_piece id board] removes a piece from the [board] at [id]*)

val put_obstacle : id -> string -> board -> unit
(** [put_obstacle id obstacle board] puts the [obstacle] on the [board]
    at [id]. *)

val remove_obstacle : id -> board -> unit
(** [remove_obstacle id board] removes the obstacle on the [board] at
    [id]. *)
