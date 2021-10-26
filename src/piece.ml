type piece_info = {
  possible_moves : (int * int) list;
  team : string;
  name : string; (* upgrade list *)
}

type piece =
  | White_Pawn of piece_info
  | Black_Pawn of piece_info
  | Rook of piece_info
  | Knight of piece_info
  | Bishop of piece_info
  | King of piece_info
  | Queen of piece_info
  | Nothing

let recordify input team = { possible_moves = []; team; name = input }

let make_piece name =
  match name with
  | "white_pawn" -> White_Pawn (recordify "white_pawn" "white")
  | "black_pawn" -> Black_Pawn (recordify "black_pawn" "black")
  | "white_rook" -> Rook (recordify "rook" "white")
  | "black_rook" -> Rook (recordify "rook" "black")
  | "white_knight" -> Knight (recordify "knight" "white")
  | "black_knight" -> Knight (recordify "knight" "black")
  | "white_bishop" -> Bishop (recordify "bishop" "white")
  | "black_bishop" -> Bishop (recordify "bishop" "black")
  | "white_king" -> King (recordify "king" "white")
  | "black_king" -> King (recordify "king" "black")
  | "white_queen" -> Queen (recordify "queen" "white")
  | "black_queen" -> Queen (recordify "queen" "black")
  | _ -> Nothing

let get_name pc =
  match pc with
  | Nothing -> "nothing"
  | White_Pawn { name; _ }
  | Black_Pawn { name; _ }
  | Rook { name; _ }
  | Bishop { name; _ }
  | Knight { name; _ }
  | King { name; _ }
  | Queen { name; _ } ->
      name

let team_of piece =
  match piece with
  | Nothing -> "nothing"
  | White_Pawn _ -> "white"
  | Black_Pawn _ -> "black"
  | Rook { team; _ } -> team
  | Bishop { team; _ } -> team
  | Knight { team; _ } -> team
  | King { team; _ } -> team
  | Queen { team; _ } -> team
