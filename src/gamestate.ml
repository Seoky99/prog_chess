open Board
open Piece_moves
open Piece

(*Idea: have a "State" object that you can continually check every loop
  iter*)

let team_error =
  "Should be impossible. Only white and black team. This is my place to\n\
   vent so here's my rant: why the hell did we make team a STRING and \
   not a VARIANT gawd"

type play_state =
  | Stalemate
  | Checkmate
  | Normal

type id = int * int

(*Note: white_castle, black_castle is not ability to castle, but is a
  check whether the rook or king has moved this game. Also note it's a
  ref but will be dereferenced when you need to check you can castle*)
type game_state = {
  game_board : board;
  mutable white_game_state : play_state;
  mutable black_game_state : play_state;
  mutable white_castle : bool ref;
  mutable black_castle : bool ref;
  mutable black_budget : int;
  mutable white_budget : int;
  mutable rounds : int;
}

(*Returns a state*)
let start_game h w =
  {
    game_board = create_board h w;
    white_game_state = Normal;
    black_game_state = Normal;
    white_castle = Piece_moves.white_castle;
    black_castle = Piece_moves.black_castle;
    black_budget = 0;
    white_budget = 0;
    rounds = 0;
  }

let get_play_state state team =
  match team with
  | "white" -> state.white_game_state
  | "black" -> state.black_game_state
  | _ -> failwith team_error

let get_budget state team =
  match team with
  | "white" -> state.white_budget
  | "black" -> state.black_budget
  | _ -> failwith team_error

let get_rounds state = state.rounds

let get_board state = state.game_board

let get_castling state team =
  match team with
  | "white" -> !(state.white_castle)
  | "black" -> !(state.black_castle)
  | _ -> failwith team_error

(*TO DO: Check stalemate, check checkmate*)
let is_checkmated state team =
  let gb = state.game_board in
  let pos_lst = positions_from_board gb in
  let possible_moves =
    calc_possible_moves pos_lst (num_cols gb) (num_rows gb)
  in

  (*A team is checkamted if they are under check and they have no
    possible moves*)
  Check.check pos_lst team (num_cols gb) (num_rows gb)
  && any_moves possible_moves pos_lst team

let is_stalemated state team =
  let gb = state.game_board in
  let pos_lst = positions_from_board gb in
  let possible_moves =
    calc_possible_moves pos_lst (num_cols gb) (num_rows gb)
  in

  (*A team is stalemated if they have no possible moves*)
  not (any_moves possible_moves pos_lst team)

let perform_state_change state team =
  match team with
  | "white" ->
      if is_checkmated state team then
        state.white_game_state <- Checkmate;
      if is_stalemated state team then
        state.white_game_state <- Stalemate
  | "black" ->
      if is_checkmated state team then
        state.black_game_state <- Checkmate;
      if is_stalemated state team then
        state.white_game_state <- Stalemate
  | _ -> failwith team_error

let rec is_in_list x lst =
  match lst with
  | [] -> false
  | h :: t -> if x = h then true else is_in_list x t

let increment_white value state =
  state.white_budget <- state.white_budget + value

let increment_black value state =
  state.black_budget <- state.black_budget + value

(*TO DO: MOVE ROOK*)
(*Fails or does nothing if nonlegal move?*)
let move_piece start_id end_id team state =
  let brd = state.game_board in
  let pos_lst = positions_from_board brd in
  let num_col = num_cols brd in
  let num_row = num_rows brd in

  let piece_at_start = piece_of_position start_id pos_lst in
  let poss_moves =
    determine_piece_possible piece_at_start start_id pos_lst num_col
      num_row
  in

  if is_in_list end_id poss_moves && team = Piece.team_of piece_at_start
  then
    let value =
      check_move_value piece_at_start start_id end_id pos_lst
    in

    (*CHECK HERE IF CASTLING*)
    let () = state.rounds <- state.rounds + 1 in

    (*If the king or rook moves, set castling ability to false*)
    let () =
      match piece_at_start with
      | Rook t when piece_info_team t = "white" ->
          Piece_moves.white_castle := false
      | King t when piece_info_team t = "black" ->
          Piece_moves.black_castle := false
      | Rook t when piece_info_team t = "white" ->
          Piece_moves.white_castle := false
      | King t when piece_info_team t = "black" ->
          Piece_moves.black_castle := false
      | _ -> ()
    in

    (*Note: all you do is affect if OTHER team is stale/checkmated*)
    match team with
    | "white" ->
        increment_white value state;
        perform_state_change state "black"
    | "black" ->
        increment_black value state;
        perform_state_change state "white"
    | _ -> failwith team_error
  else failwith "Nonlegal move"
