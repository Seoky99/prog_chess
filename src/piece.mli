type piece_info
(** Abstract type of values representing a chess piece. *)

type piece =
  | White_Pawn of piece_info
  | Black_Pawn of piece_info
  | Rook of piece_info
  | Knight of piece_info
  | Bishop of piece_info
  | King of piece_info
  | Queen of piece_info
  | Nothing

val make_piece : string -> piece

val get_name : piece -> string

val team_of : piece -> string
(** [team_of piece] is the team of piece, either "white" or "black" *)
