(* HOW TO USE GAME STATE (DELETE LATER): Game state has information of
   which teams are checkmated, which teams are stalemated, the budgets
   of each teams. Game state allows you to move pieces and change
   budgets*)

type game_state

type id = int * int

type play_state =
  | Stalemate
  | Checkmate
  | Normal
(* Play state describes the *)

val start_game : int -> int -> game_state
(** [start_game h w] creates a state with information of board of height
    h and width w. The budget is set to 0 for both teams. Round is set
    to 0. Both the states of the teams are Normal.*)

val get_play_state : game_state -> string -> play_state
(** [get_play_state state team] returns the play state of the team
    inputed in. Basically a check whether a team is checkmated,
    stalemated, or not.*)

val move_piece : id -> id -> string -> game_state -> unit
(** [move_piece start_id end_id team state] does a lot of things. 1)
    Moves the piece at start_id to end_id, modifying the state's board
    to reflect this. If there is no piece there this function does
    nothing. 2) If there was a legal move performed, [team's] budget
    increases by the value of the piece taken. Note: if no piece was at
    end_id then budget is incremented by 0. 3) Modifies the state of the
    opposite team to a play_state. 4) Increments the round counter by 1. *)
