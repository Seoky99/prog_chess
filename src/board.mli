(** Abstract type of values representing a chess board.*)
type board 

(** [board_from_json json] is the chess board that [json] represents.contents
Requires: [json] is a rectangular representation.*)
val board_from_json : Yojson.Basic.t -> board 