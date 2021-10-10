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

(** [id_lst pos_lst] produces a list of ids from a list of positions *)
val id_lst : position list -> id list 