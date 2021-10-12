(** Abstract type of values representing a chess board.
NOTE: REPLACE NAME TO t once we are familiar with it*)
type board 

(**Abstract type of values representing a chess square*)
type position

(** Where the chess square location is*)
type id = int * int

(** [board_from_json json] is the chess board that [json] represents.contents
Requires: [json] is a rectangular representation.*)
val board_from_json : Yojson.Basic.t -> board 

(** [n_row n board] is the nth row of the chess board
Requires: [n] >= 1*)
val n_row : int -> board -> position list  

(** [n_row n board] is the nth column of the chess board
Requires: [n] >= 1*)
val n_col : int -> board -> position list 

(** [id_lst id_pos_lst] produces a list of ids from a list of positions *)
val id_pos_lst : position list -> id list 

(** [id_board board]* produces a list of id lists from a board, essentially
mapping out the whole board *)
val id_board : board -> id list list 

(** [get_color board id] the ouput is the color of the board at that board with the given id
Requires: id is a valid location on the board*)
val get_color: board-> id -> string