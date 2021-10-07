open OUnit2 

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input

let board_tests = [
    make_f_test "First test" 2 2; 
  
]



let suite = 
  "test suite for Chess game"
  >::: List.flatten [board_tests]

let _ = run_test_tt_main suite 