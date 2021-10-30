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
(** [make_piece name] makes a piece with [name]. Valid names are
    color_piecename, and nothing. Example: white_rook or black_king or
    nothing Raises: Failure if input is invalid name. *)

val get_name : piece -> string
(** [get_name pc] is the name of the [pc] inputted*)

val get_value : piece -> int
(** [get_name pc] is the value of the [pc] inputted*)

val team_of : piece -> string
(** [team_of piece] is the team of piece, either "white" or "black" *)

val piece_info_team : piece_info -> string
(** [piece_info_team pi] is the team of that piece_info *)
