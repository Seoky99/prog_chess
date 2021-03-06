val create_board_string : int -> int -> int -> int -> string
(** [create_board_string height width r c] Creates an string
    representation [height] x [width] chess board json, starting at
    position ([r],[c]) with the template: { "name": [r, c], "positions":
    [ { 'id': (rxc), 'obstacle': 'none or some' 'color': 'white or
    black' 'piece': <depends on piece> }, ] } Note: single string
    markers will be replaced by double quotes. Requires: height > 4 and
    width > 8 *)
