open OUnit2 
open Chess
open Board 
open Author 
open Yojson.Basic.Util 

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input


(* TODO: Test board to check for correctness *)
(* TODO: Create printer function for testing output of board*)
let board = board_from_json (Yojson.Basic.from_file "data/3x3.json")

let board_tests = [
    make_f_test "Board from json 3x3" 2 2; 
  
]

let suite = 
  "test suite for Chess game"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 