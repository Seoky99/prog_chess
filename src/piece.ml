type piece_info = {
  possible_moves : (int * int) list;
  team : string;
  name : string;
  (*TODO: Implement upgrade list*)
  value : int;
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

let recordify name team value =
  { possible_moves = []; name; team; value }

let make_piece name =
  match name with
  | "white_pawn" -> White_Pawn (recordify "white_pawn" "white" 1)
  | "black_pawn" -> Black_Pawn (recordify "black_pawn" "black" 1)
  | "white_rook" -> Rook (recordify "rook" "white" 5)
  | "black_rook" -> Rook (recordify "rook" "black" 5)
  | "white_knight" -> Knight (recordify "knight" "white" 3)
  | "black_knight" -> Knight (recordify "knight" "black" 3)
  | "white_bishop" -> Bishop (recordify "bishop" "white" 3)
  | "black_bishop" -> Bishop (recordify "bishop" "black" 3)
  | "white_king" -> King (recordify "king" "white" 0)
  | "black_king" -> King (recordify "king" "black" 0)
  | "white_queen" -> Queen (recordify "queen" "white" 9)
  | "black_queen" -> Queen (recordify "queen" "black" 9)
  | "nothing" -> Nothing
  | _ -> failwith "Not valid piece name"

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

let get_value pc =
  match pc with
  | Nothing -> 0
  | White_Pawn { value; _ }
  | Black_Pawn { value; _ }
  | Rook { value; _ }
  | Bishop { value; _ }
  | Knight { value; _ }
  | King { value; _ }
  | Queen { value; _ } ->
      value

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

let piece_info_team pi =
  match pi with
  | { team; _ } -> team

let money_increment_get piece team =
  let black = ref 0 and white = ref 0 in

  if team = "black" then (
    black := !black + get_value piece;
    !black)
  else (
    white := !white + get_value piece;
    !white)
