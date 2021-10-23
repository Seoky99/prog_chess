open OUnit2 
open Chess 
open Board 
open Jsongen

 (*Now outdated, can keep if we ever decide to support smaller boards. *)
let board3x3 = board_from_json (Yojson.Basic.from_file "data/3x3.json")
let board3x4 = board_from_json (Yojson.Basic.from_file "data/3x4.json")
let board4x3 = board_from_json (Yojson.Basic.from_file "data/4x3.json")

(*Board tests*)
let board8x8 = board_from_json (Yojson.Basic.from_string (create_board 8 8 1 1))

let board8x10 = board_from_json (Yojson.Basic.from_string (create_board 8 10 1 1))

let board8x11 = board_from_json (Yojson.Basic.from_string (create_board 8 11 1 1))

let board11x8 = board_from_json (Yojson.Basic.from_string (create_board 11 8 1 1))

(*TODO: Store these somewhere else?*)
let row1board3x3 = n_row 1 board3x3  
let row2board3x3 = n_row 2 board3x3  
let row3board3x3 = n_row 3 board3x3  
let col1board3x3 = n_col 1 board3x3 
let col2board3x3 = n_col 2 board3x3 
let col3board3x3 = n_col 3 board3x3 

(*DELETE MUCH LATER*)
let row1board8x8 = n_row 1 board8x8  
let row2board8x8 = n_row 2 board8x8  
let row3board8x8 = n_row 3 board8x8  
let col1board8x8 = n_col 1 board8x8 
let col2board8x8 = n_col 2 board8x8 
let col3board8x8 = n_col 3 board8x8 

let rec tuple_printer tlst = 
  match tlst with 
  | [] -> ""
  | (x,y) :: t -> "(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") " ^ tuple_printer t

let rec one_d_printer str_list = 
  match str_list with 
  | [] -> "" 
  | h :: t -> h ^ ";" ^ one_d_printer t 

let rec two_d_printer str_lst =
  match str_lst with 
   [] -> ""
   | h :: t -> "[" ^ (one_d_printer h) ^ "];" ^ two_d_printer t

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input
  
let make_color_test name expected_output input = name>:: fun _ -> assert_equal expected_output input ~printer: String.escaped

let make_obstacle_test name expected_output input =name>:: fun _ -> assert_equal expected_output input ~printer: String.escaped

let make_2d_test name expected_output input =name>:: fun _ -> assert_equal expected_output input ~printer: two_d_printer

let board_tests = [

    (*Move these around later, testing json generator completely.*)
    make_f_test "Row 1 of Board 8x8" [(1,1); (1,2); (1,3); (1,4); (1,5); (1,6); (1,7); (1,8)] (id_pos_lst row1board8x8); 
    make_f_test "Row 2 of Board 8x8" [(2,1); (2,2); (2,3); (2,4); (2,5); (2,6); (2,7); (2,8)] (id_pos_lst row2board8x8); 
    make_f_test "Row 3 of Board 8x8" [(3,1); (3,2); (3,3); (3,4); (3,5); (3,6); (3,7); (3,8)] (id_pos_lst row3board8x8); 
    make_f_test "Col 1 of Board 8x8" [(1,1); (2,1); (3,1); (4,1); (5,1); (6,1); (7,1); (8,1)] (id_pos_lst col1board8x8); 
    make_f_test "Col 2 of Board 8x8" [(1,2); (2,2); (3,2); (4,2); (5,2); (6,2); (7,2); (8,2)] (id_pos_lst col2board8x8);
    make_f_test "Col 3 of Board 8x8" [(1,3); (2,3); (3,3); (4,3); (5,3); (6,3); (7,3); (8,3)] (id_pos_lst col3board8x8);

    make_2d_test "Board 8x8 pieces are correct" [["rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"]; ["black_pawn";"black_pawn"; "black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn";]; ["rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"]] (piece_board board8x8);
    make_2d_test "Board 8x10 pieces are correct" [["nothing";"rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"; "nothing"]; ["black_pawn";"black_pawn";"black_pawn"; "black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn"; "black_pawn"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn";]; ["nothing";"rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"; "nothing"]] (piece_board board8x10);
    make_2d_test "Board 8x11 pieces are correct" [["nothing";"rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"; "nothing"; "nothing"]; ["black_pawn";"black_pawn";"black_pawn"; "black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn"; "black_pawn"; "black_pawn"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing";"nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"]; ["nothing";"rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"; "nothing"; "nothing"]] (piece_board board8x11);
    make_2d_test "Board 11x8 pieces are correct" [["rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"]; ["black_pawn";"black_pawn"; "black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";"black_pawn";]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"];["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"];["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"; "nothing"]; ["white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn"; "white_pawn";]; ["rook"; "knight"; "bishop"; "queen"; "king"; "bishop"; "knight"; "rook"]] (piece_board board11x8);
    
    (*Testing id_pos_lst, n_row, and n_col *)
    make_f_test "Row 1 of Board 3x3" [(1,1); (1,2); (1,3)] (id_pos_lst row1board3x3); 
    make_f_test "Row 2 of Board 3x3" [(2,1); (2,2); (2,3)] (id_pos_lst row2board3x3); 
    make_f_test "Row 3 of Board 3x3" [(3,1); (3,2); (3,3)] (id_pos_lst row3board3x3); 
    make_f_test "Col 1 of Board 3x3" [(1,1); (2,1); (3,1)] (id_pos_lst col1board3x3); 
    make_f_test "Col 2 of Board 3x3" [(1,2); (2,2); (3,2)] (id_pos_lst col2board3x3);
    make_f_test "Col 3 of Board 3x3" [(1,3); (2,3); (3,3)] (id_pos_lst col3board3x3);

    (*Testing a board that is 4x3 so that the board is not square but rectangular*)
    make_f_test "Row 1 of Board 4x3" [(1,1); (1,2) ;(1,3) ] (id_pos_lst (n_row 1 board4x3));
    make_f_test "Row 2 of the Board 4x3" [(2,1); (2,2); (2,3)] (id_pos_lst (n_row 2 board4x3));
    make_f_test "Row 3 of Board 4x3" [(3,1); (3,2); (3,3)] (id_pos_lst (n_row 3 board4x3));
    make_f_test "Row 4 of Board 4x3" [(4,1); (4,2); (4,3)] (id_pos_lst (n_row 4 board4x3));
    make_f_test "Col 1 of Board 4x3" [(1,1); (2,1); (3,1); (4,1)] (id_pos_lst (n_col 1 board4x3));
    make_f_test "Col 2 of Board 4x3" [(1,2); (2,2); (3,2); (4,2)] (id_pos_lst (n_col 2 board4x3));
    make_f_test "Col 3 of Board 4x3" [(1,3); (2,3); (3,3); (4,3)] (id_pos_lst (n_col 3 board4x3));

    (*Testing a board that is 3x4 so that the board is not square but this time there is more columns*)
    make_f_test "Row 1 of Board 3x4" [(1,1); (1,2) ;(1,3);(1,4) ] (id_pos_lst (n_row 1 board3x4));
    make_f_test "Row 2 of the Board 3x4" [(2,1); (2,2); (2,3);(2,4)] (id_pos_lst (n_row 2 board3x4));
    make_f_test "Row 3 of Board 3x4" [(3,1); (3,2); (3,3);(3,4)] (id_pos_lst (n_row 3 board3x4));
    make_f_test "Col 1 of Board 3x4" [(1,1); (2,1); (3,1)] (id_pos_lst (n_col 1 board3x4));
    make_f_test "Col 2 of Board 3x4" [(1,2); (2,2); (3,2)] (id_pos_lst (n_col 2 board3x4));
    make_f_test "Col 3 of Board 3x4" [(1,3);(2,3);(3,3)] (id_pos_lst (n_col 3 board3x4));
    make_f_test "Col 4 of Board 3x4" [(1,4);(2,4);(3,4)] (id_pos_lst (n_col 4 board3x4));
    
    (* Testing id_board *)
    make_f_test "Board 3x3 is correct" [[(1,1); (1,2); (1,3)]; [(2,1); (2,2); (2,3)]; [(3,1); (3,2); (3,3)]] (id_board board3x3); 
    make_f_test "Board 4x3 is correct" [[(1,1); (1,2); (1,3)]; [(2,1); (2,2); (2,3)]; [(3,1); (3,2); (3,3)]; [(4,1); (4,2); (4,3)]] (id_board board4x3); 
    make_f_test "Board 3x4 is correct" [[(1,1); (1,2); (1,3); (1,4)]; [(2,1); (2,2); (2,3); (2,4)]; [(3,1); (3,2); (3,3); (3,4)]] (id_board board3x4); 

    (* Testing pieces on board*)
    make_f_test "Board 3x3 pieces are correct" [["nothing"; "nothing"; "white_pawn"]; ["black_pawn";"nothing"; "bishop"]; ["bishop"; "king"; "king"]] (piece_board board3x3); 
    make_f_test "Board 3x3 pieces are correct" [["nothing"; "nothing"; "white_pawn"]; ["black_pawn";"nothing"; "bishop"]; ["bishop"; "king"; "king"]] (piece_board board3x3); 
    
    (* Testing the function get_color *)
    make_color_test "Testing Board 3x3 and location (1,1)" "white" (get_color board3x3 (1,1));
    make_color_test "Testing Board 3x3 and location (3,3)" "white" (get_color board3x3 (3,3));
    make_color_test "Testing Board 3x3 and location (1,3)" "white" (get_color board3x3 (1,3));
    make_color_test "Testing Board 3x3 and location (3,1)" "white" (get_color board3x3 (3,1));
    make_color_test "Testing Board 3x3 and location (2,3)" "black" (get_color board3x3 (2,3));
    make_color_test "Testing Board 3x3 and location (3,2)" "black" (get_color board3x3 (3,2));

    (*Testing the function get_obstacles*)

    make_obstacle_test "Testing board 3x3 and location (1,1)" "none" (get_obstacle board3x3 (1,1));
    make_obstacle_test "Testing board 3x3 and location (3,3)" "none" (get_obstacle board3x3 (3,3));
    make_obstacle_test "Testing board 3x3 and location (1,3)" "none" (get_obstacle board3x3 (1,3));
    make_obstacle_test "Testing board 3x3 and location (3,1)" "none" (get_obstacle board3x3 (3,1));
    make_obstacle_test "Testing board 3x3 and location (2,3)" "none" (get_obstacle board3x3 (2,3));
    make_obstacle_test "Testing board 3x3 and location (3,2)" "none" (get_obstacle board3x3 (3,2));
]

let suite = 
  "test suite for Chess game 123"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 