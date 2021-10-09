open OUnit2 
open Chess
open Board 
open Yojson.Basic.Util 

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input



let asd = board_from_json (Yojson.Basic.from_file "data/3x3.json")

let blah = testplz 2 

let board_tests = [
    make_f_test "Board from json 3x3" 2 2; 
  
]



let suite = 
  "test suite for Chess game"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 