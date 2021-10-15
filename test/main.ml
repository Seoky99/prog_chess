open OUnit2 
open Chess 
open Board 

(* TODO: Test board to check for correctness *)
(* TODO: Create helpful printer function for testing output of board*)
let board3x3 = board_from_json (Yojson.Basic.from_file "data/3x3.json")
let board3x4 = board_from_json (Yojson.Basic.from_file "data/3x4.json")
let board4x3 = board_from_json (Yojson.Basic.from_file "data/4x3.json")
let board8x8 = board_from_json (Yojson.Basic.from_file "data/8x8.json")

(*TODO: Store these somewhere else?*)
let row1board3x3 = n_row 1 board3x3  
let row2board3x3 = n_row 2 board3x3  
let row3board3x3 = n_row 3 board3x3  
let col1board3x3 = n_col 1 board3x3 
let col2board3x3 = n_col 2 board3x3 
let col3board3x3 = n_col 3 board3x3 


let rec tuple_printer tlst = 
  match tlst with 
  | [] -> ""
  | (x,y) :: t -> "(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") " ^ tuple_printer t

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input 
  
let make_color_test name expected_output input = name>:: fun _ -> assert_equal expected_output input ~printer: String.escaped

let make_obstacle_test name expected_output input =name>:: fun _ -> assert_equal expected_output input ~printer: String.escaped

let board_tests = [

    (* Testing id_pos_lst, n_row, and n_col *)
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
  "test suite for Chess game"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 