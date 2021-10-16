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

let x = Piece.test 


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
  

let board_tests = [

    (*Testing id_pos_lst, n_row, and n_col *)
    make_f_test "Row 1 of Board 3x3" [(1,1); (1,2); (1,3)] (id_pos_lst row1board3x3); 
    make_f_test "Row 2 of Board 3x3" [(2,1); (2,2); (2,3)] (id_pos_lst row2board3x3); 
    make_f_test "Row 3 of Board 3x3" [(3,1); (3,2); (3,3)] (id_pos_lst row3board3x3); 
    make_f_test "Col 1 of Board 3x3" [(1,1); (2,1); (3,1)] (id_pos_lst col1board3x3); 
    make_f_test "Col 2 of Board 3x3" [(1,2); (2,2); (3,2)] (id_pos_lst col2board3x3);
    make_f_test "Col 3 of Board 3x3" [(1,3); (2,3); (3,3)] (id_pos_lst col3board3x3);

    (* Testing id_board *)
    make_f_test "Board 3x3 is correct" [[(1,1); (1,2); (1,3)]; [(2,1); (2,2); (2,3)]; [(3,1); (3,2); (3,3)]] (id_board board3x3); 
    make_f_test "Board 4x3 is correct" [[(1,1); (1,2); (1,3)]; [(2,1); (2,2); (2,3)]; [(3,1); (3,2); (3,3)]; [(4,1); (4,2); (4,3)]] (id_board board4x3); 
    make_f_test "Board 3x4 is correct" [[(1,1); (1,2); (1,3); (1,4)]; [(2,1); (2,2); (2,3); (2,4)]; [(3,1); (3,2); (3,3); (3,4)]] (id_board board3x4); 

    (* Testing pieces on board*)
    make_f_test "Board 3x3 pieces are correct" [["nothing"; "nothing"; "white_pawn"]; ["black_pawn";"nothing"; "bishop"]; ["bishop"; "king"; "king"]] (piece_board board3x3); 
    make_f_test "Board 3x3 pieces are correct" [["nothing"; "nothing"; "white_pawn"]; ["black_pawn";"nothing"; "bishop"]; ["bishop"; "king"; "king"]] (piece_board board3x3); 
    
    
]

let suite = 
  "test suite for Chess game"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 