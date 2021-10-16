open Board
open Graphics
open Yojson.Basic

let window = open_graph " 800x800"
let window_name = set_window_title "something cool"
let board (json : string) : board = board_from_json(from_file json)
let board_height (board : board) : int = (num_rows board) * 60
let board_width (board : board) : int = (num_cols board) * 60




