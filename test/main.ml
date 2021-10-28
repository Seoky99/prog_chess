open OUnit2
open Chess
open Board
open Jsongen
open Piece
open Piece_moves

(*Now outdated, can keep if we ever decide to support smaller boards. *)
let board3x3 = board_from_json (Yojson.Basic.from_file "data/3x3.json")

let board3x4 = board_from_json (Yojson.Basic.from_file "data/3x4.json")

let board4x3 = board_from_json (Yojson.Basic.from_file "data/4x3.json")

(*Board tests*)
let board8x8 =
  board_from_json (Yojson.Basic.from_string (create_board 8 8 1 1))

let board8x10 =
  board_from_json (Yojson.Basic.from_string (create_board 8 10 1 1))

let board8x11 =
  board_from_json (Yojson.Basic.from_string (create_board 8 11 1 1))

let board11x8 =
  board_from_json (Yojson.Basic.from_string (create_board 11 8 1 1))

(*This 8x8 board is modified by: first, white_pawn placed in (3,3) then,
  white_queen is placed on (2,1) then, a white_pawn is removed from
  (7,1)*)
let modified8x8 =
  put_piece (3, 3) (make_piece "white_pawn") board8x8
  |> put_piece (2, 1) (make_piece "white_queen")
  |> remove_piece (7, 1)

(*This 8x8 is modified by: first, white_queen is placed on (3,3) then,
  white_queen is removed on (3,3)*)
let placeandremove8x8 =
  put_piece (3, 3) (make_piece "white_queen") board8x8
  |> remove_piece (3, 3)

let rooktestingboard =
  put_piece (3, 3) (make_piece "white_rook") board8x8
  |> put_piece (3, 4) (make_piece "black_rook")
  |> put_piece (6, 3) (make_piece "black_rook")

let bishoptestingboard =
  put_piece (3, 2) (make_piece "black_bishop") board8x8
  |> put_piece (4, 4) (make_piece "black_bishop")
  |> put_piece (5, 4) (make_piece "black_bishop")
  |> put_piece (5, 5) (make_piece "white_bishop")

let queentestingboard =
  put_piece (3, 1) (make_piece "black_queen") board8x8
  |> put_piece (5, 3) (make_piece "white_queen")
  |> put_piece (5, 5) (make_piece "white_queen")

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

let tuple_printer t =
  match t with
  | x, y -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let rec tuple_lst_printer tlst =
  match tlst with
  | [] -> ""
  | (x, y) :: t ->
      "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ") "
      ^ tuple_lst_printer t

let rec one_d_printer str_list =
  match str_list with
  | [] -> ""
  | h :: t -> h ^ ";" ^ one_d_printer t

let rec two_d_printer str_lst =
  match str_lst with
  | [] -> ""
  | h :: t -> "[" ^ one_d_printer h ^ "];" ^ two_d_printer t

let make_f_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input

let make_tuple_lst_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:tuple_lst_printer

let make_tuple_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:tuple_printer

let make_string_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:String.escaped

let make_color_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:String.escaped

let make_obstacle_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:String.escaped

let make_1d_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:one_d_printer

let make_2d_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output input ~printer:two_d_printer

let board_tests =
  [
    (*Move these around later, testing json generator completely.*)
    make_f_test "Row 1 of Board 8x8"
      [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (1, 8) ]
      (id_pos_lst row1board8x8);
    make_f_test "Row 2 of Board 8x8"
      [ (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7); (2, 8) ]
      (id_pos_lst row2board8x8);
    make_f_test "Row 3 of Board 8x8"
      [ (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); (3, 6); (3, 7); (3, 8) ]
      (id_pos_lst row3board8x8);
    make_f_test "Col 1 of Board 8x8"
      [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1); (6, 1); (7, 1); (8, 1) ]
      (id_pos_lst col1board8x8);
    make_f_test "Col 2 of Board 8x8"
      [ (1, 2); (2, 2); (3, 2); (4, 2); (5, 2); (6, 2); (7, 2); (8, 2) ]
      (id_pos_lst col2board8x8);
    make_f_test "Col 3 of Board 8x8"
      [ (1, 3); (2, 3); (3, 3); (4, 3); (5, 3); (6, 3); (7, 3); (8, 3) ]
      (id_pos_lst col3board8x8);
    make_2d_test "Board 8x8 pieces are correct"
      [
        [
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
        ];
        [
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
        ];
        [
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
        ];
      ]
      (piece_board board8x8);
    make_2d_test "Board 8x10 pieces are correct"
      [
        [
          "nothing";
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
          "nothing";
        ];
        [
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
        ];
        [
          "nothing";
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
          "nothing";
        ];
      ]
      (piece_board board8x10);
    make_2d_test "Board 8x11 pieces are correct"
      [
        [
          "nothing";
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
          "nothing";
          "nothing";
        ];
        [
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
        ];
        [
          "nothing";
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
          "nothing";
          "nothing";
        ];
      ]
      (piece_board board8x11);
    make_2d_test "Board 11x8 pieces are correct"
      [
        [
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
        ];
        [
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
          "black_pawn";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
          "nothing";
        ];
        [
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
          "white_pawn";
        ];
        [
          "rook";
          "knight";
          "bishop";
          "queen";
          "king";
          "bishop";
          "knight";
          "rook";
        ];
      ]
      (piece_board board11x8);
    (*Testing id_pos_lst, n_row, and n_col *)
    make_f_test "Row 1 of Board 3x3"
      [ (1, 1); (1, 2); (1, 3) ]
      (id_pos_lst row1board3x3);
    make_f_test "Row 2 of Board 3x3"
      [ (2, 1); (2, 2); (2, 3) ]
      (id_pos_lst row2board3x3);
    make_f_test "Row 3 of Board 3x3"
      [ (3, 1); (3, 2); (3, 3) ]
      (id_pos_lst row3board3x3);
    make_f_test "Col 1 of Board 3x3"
      [ (1, 1); (2, 1); (3, 1) ]
      (id_pos_lst col1board3x3);
    make_f_test "Col 2 of Board 3x3"
      [ (1, 2); (2, 2); (3, 2) ]
      (id_pos_lst col2board3x3);
    make_f_test "Col 3 of Board 3x3"
      [ (1, 3); (2, 3); (3, 3) ]
      (id_pos_lst col3board3x3);
    (*Testing a board that is 4x3 so that the board is not square but
      rectangular*)
    make_f_test "Row 1 of Board 4x3"
      [ (1, 1); (1, 2); (1, 3) ]
      (id_pos_lst (n_row 1 board4x3));
    make_f_test "Row 2 of the Board 4x3"
      [ (2, 1); (2, 2); (2, 3) ]
      (id_pos_lst (n_row 2 board4x3));
    make_f_test "Row 3 of Board 4x3"
      [ (3, 1); (3, 2); (3, 3) ]
      (id_pos_lst (n_row 3 board4x3));
    make_f_test "Row 4 of Board 4x3"
      [ (4, 1); (4, 2); (4, 3) ]
      (id_pos_lst (n_row 4 board4x3));
    make_f_test "Col 1 of Board 4x3"
      [ (1, 1); (2, 1); (3, 1); (4, 1) ]
      (id_pos_lst (n_col 1 board4x3));
    make_f_test "Col 2 of Board 4x3"
      [ (1, 2); (2, 2); (3, 2); (4, 2) ]
      (id_pos_lst (n_col 2 board4x3));
    make_f_test "Col 3 of Board 4x3"
      [ (1, 3); (2, 3); (3, 3); (4, 3) ]
      (id_pos_lst (n_col 3 board4x3));
    (*Testing a board that is 3x4 so that the board is not square but
      this time there is more columns*)
    make_f_test "Row 1 of Board 3x4"
      [ (1, 1); (1, 2); (1, 3); (1, 4) ]
      (id_pos_lst (n_row 1 board3x4));
    make_f_test "Row 2 of the Board 3x4"
      [ (2, 1); (2, 2); (2, 3); (2, 4) ]
      (id_pos_lst (n_row 2 board3x4));
    make_f_test "Row 3 of Board 3x4"
      [ (3, 1); (3, 2); (3, 3); (3, 4) ]
      (id_pos_lst (n_row 3 board3x4));
    make_f_test "Col 1 of Board 3x4"
      [ (1, 1); (2, 1); (3, 1) ]
      (id_pos_lst (n_col 1 board3x4));
    make_f_test "Col 2 of Board 3x4"
      [ (1, 2); (2, 2); (3, 2) ]
      (id_pos_lst (n_col 2 board3x4));
    make_f_test "Col 3 of Board 3x4"
      [ (1, 3); (2, 3); (3, 3) ]
      (id_pos_lst (n_col 3 board3x4));
    make_f_test "Col 4 of Board 3x4"
      [ (1, 4); (2, 4); (3, 4) ]
      (id_pos_lst (n_col 4 board3x4));
    (* Testing id_board *)
    make_f_test "Board 3x3 is correct"
      [
        [ (1, 1); (1, 2); (1, 3) ];
        [ (2, 1); (2, 2); (2, 3) ];
        [ (3, 1); (3, 2); (3, 3) ];
      ]
      (id_board board3x3);
    make_f_test "Board 4x3 is correct"
      [
        [ (1, 1); (1, 2); (1, 3) ];
        [ (2, 1); (2, 2); (2, 3) ];
        [ (3, 1); (3, 2); (3, 3) ];
        [ (4, 1); (4, 2); (4, 3) ];
      ]
      (id_board board4x3);
    make_f_test "Board 3x4 is correct"
      [
        [ (1, 1); (1, 2); (1, 3); (1, 4) ];
        [ (2, 1); (2, 2); (2, 3); (2, 4) ];
        [ (3, 1); (3, 2); (3, 3); (3, 4) ];
      ]
      (id_board board3x4);
    (* Testing pieces on board*)
    make_f_test "Board 3x3 pieces are correct"
      [
        [ "nothing"; "nothing"; "white_pawn" ];
        [ "black_pawn"; "nothing"; "bishop" ];
        [ "bishop"; "king"; "king" ];
      ]
      (piece_board board3x3);
    make_f_test "Board 3x3 pieces are correct"
      [
        [ "nothing"; "nothing"; "white_pawn" ];
        [ "black_pawn"; "nothing"; "bishop" ];
        [ "bishop"; "king"; "king" ];
      ]
      (piece_board board3x3);
    (*Testing id from position*)
    make_f_test "First element of 2nd row of 8x8 is (2,1)" (2, 1)
      (id_from_position (List.hd row2board8x8));
    (* Testing the function get_color *)
    make_string_test "Testing Board 3x3 and location (1,1)" "white"
      (get_color board3x3 (1, 1));
    make_string_test "Testing Board 3x3 and location (3,3)" "white"
      (get_color board3x3 (3, 3));
    make_string_test "Testing Board 3x3 and location (1,3)" "white"
      (get_color board3x3 (1, 3));
    make_string_test "Testing Board 3x3 and location (3,1)" "white"
      (get_color board3x3 (3, 1));
    make_string_test "Testing Board 3x3 and location (2,3)" "black"
      (get_color board3x3 (2, 3));
    make_string_test "Testing Board 3x3 and location (3,2)" "black"
      (get_color board3x3 (3, 2));
    (*Testing the function get_obstacles*)
    make_string_test "Testing board 3x3 and location (1,1)" "none"
      (get_obstacle board3x3 (1, 1));
    make_string_test "Testing board 3x3 and location (3,3)" "none"
      (get_obstacle board3x3 (3, 3));
    make_string_test "Testing board 3x3 and location (1,3)" "none"
      (get_obstacle board3x3 (1, 3));
    make_string_test "Testing board 3x3 and location (3,1)" "none"
      (get_obstacle board3x3 (3, 1));
    make_string_test "Testing board 3x3 and location (2,3)" "none"
      (get_obstacle board3x3 (2, 3));
    make_string_test "Testing board 3x3 and location (3,2)" "none"
      (get_obstacle board3x3 (3, 2));
    (*Testing function piece_of_position*)
    make_string_test "(2,1) of Board 8x8 is black pawn" "black_pawn"
      (get_name
         (piece_of_position (2, 1) (positions_from_board board8x8)));
    make_string_test "(7,1) of Board 8x8 is white pawn" "white_pawn"
      (get_name
         (piece_of_position (7, 1) (positions_from_board board8x8)));
    make_string_test "(1,1) of Board 8x8 is rook" "rook"
      (get_name
         (piece_of_position (1, 1) (positions_from_board board8x8)));
    make_string_test "(1,2) of Board 8x8 is knight" "knight"
      (get_name
         (piece_of_position (1, 2) (positions_from_board board8x8)));
    make_string_test "(1,3) of Board 8x8 is bishop" "bishop"
      (get_name
         (piece_of_position (1, 3) (positions_from_board board8x8)));
    make_string_test "(1,4) of Board 8x8 is queen" "queen"
      (get_name
         (piece_of_position (1, 4) (positions_from_board board8x8)));
    (*Testing function put_piece*)
    make_string_test "A white pawn is placed in (3x3)" "white_pawn"
      (get_name
         (piece_of_position (3, 3) (positions_from_board modified8x8)));
    make_string_test
      "A white queen is placed on (2x1), where a black pawn is" "queen"
      (get_name
         (piece_of_position (2, 1) (positions_from_board modified8x8)));
    (*Testing function remove_piece*)
    make_string_test "A white pawn is removed on (7,1)" "nothing"
      (get_name
         (piece_of_position (7, 1) (positions_from_board modified8x8)));
    make_string_test "A white queen is added then removed on (3,3)"
      "nothing"
      (get_name
         (piece_of_position (3, 3)
            (positions_from_board placeandremove8x8)));
  ]

(* Pieces for testing movement *)
let c1black_pawn =
  piece_of_position (2, 1) (positions_from_board board8x8)

let c2black_pawn =
  piece_of_position (2, 2) (positions_from_board board8x8)

let c1white_pawn =
  piece_of_position (7, 1) (positions_from_board board8x8)

let c2white_pawn =
  piece_of_position (7, 2) (positions_from_board board8x8)

let rook81 = piece_of_position (8, 1) (positions_from_board board8x8)

let rook33 =
  piece_of_position (3, 3) (positions_from_board rooktestingboard)

let rook34 =
  piece_of_position (3, 4) (positions_from_board rooktestingboard)

let rook63 =
  piece_of_position (6, 3) (positions_from_board rooktestingboard)

let bishop83 = piece_of_position (8, 3) (positions_from_board board8x8)

let bishop32 =
  piece_of_position (3, 2) (positions_from_board bishoptestingboard)

let bishop44 =
  piece_of_position (4, 4) (positions_from_board bishoptestingboard)

let bishop54 =
  piece_of_position (5, 4) (positions_from_board bishoptestingboard)

let bishop55 =
  piece_of_position (5, 5) (positions_from_board bishoptestingboard)

let queen84 = piece_of_position (8, 4) (positions_from_board board8x8)

let queen31 =
  piece_of_position (3, 1) (positions_from_board queentestingboard)

let queen53 =
  piece_of_position (5, 3) (positions_from_board queentestingboard)

let queen55 =
  piece_of_position (5, 5) (positions_from_board queentestingboard)

let move_piece_tests =
  [
    (*TESTING PAWNS*)
    (*Testing the individual white pawn moveset *)
    make_tuple_lst_test "Moving left white pawn" [ (5, 1); (6, 1) ]
      (determine_piece_possible c1white_pawn (7, 1)
         (positions_from_board board8x8)
         8 8);
    make_tuple_lst_test "Moving c2 white pawn" [ (5, 2); (6, 2) ]
      (determine_piece_possible c1white_pawn (7, 2)
         (positions_from_board board8x8)
         8 8);
    (*Testing the individual black pawn moveset *)
    make_tuple_lst_test "Moving left black pawn" [ (4, 1); (3, 1) ]
      (determine_piece_possible c1black_pawn (2, 1)
         (positions_from_board board8x8)
         8 8);
    make_tuple_lst_test "Moving c2 black pawn" [ (4, 2); (3, 2) ]
      (determine_piece_possible c1black_pawn (2, 2)
         (positions_from_board board8x8)
         8 8);
    (*White pawn moves test*)
    make_tuple_lst_test "White pawns" [ (5, 1); (6, 1) ]
      (white_pawn_moves (7, 1) (positions_from_board board8x8) 8 8);
    (*Black pawn moves test*)
    make_tuple_lst_test "Black pawns" [ (4, 1); (3, 1) ]
      (black_pawn_moves (2, 1) (positions_from_board board8x8) 8 8);
    (*TESTING ROOKS*)
    make_tuple_lst_test "Make sure left white rook cannot move" []
      (determine_piece_possible rook81 (8, 1)
         (positions_from_board board8x8)
         8 8);
    make_tuple_lst_test "Move set for rook 33"
      [ (3, 4); (3, 2); (3, 1); (4, 3); (5, 3); (6, 3); (2, 3) ]
      (determine_piece_possible rook33 (3, 3)
         (positions_from_board rooktestingboard)
         8 8);
    make_tuple_lst_test "Move set rook 34"
      [
        (3, 5);
        (3, 6);
        (3, 7);
        (3, 8);
        (3, 3);
        (4, 4);
        (5, 4);
        (6, 4);
        (7, 4);
      ]
      (determine_piece_possible rook34 (3, 4)
         (positions_from_board rooktestingboard)
         8 8);
    make_tuple_lst_test "Move set for rook 63"
      [
        (6, 4);
        (6, 5);
        (6, 6);
        (6, 7);
        (6, 8);
        (6, 2);
        (6, 1);
        (7, 3);
        (5, 3);
        (4, 3);
        (3, 3);
      ]
      (determine_piece_possible rook63 (6, 3)
         (positions_from_board rooktestingboard)
         8 8);
    (*TESTING BISHOPS*)
    make_tuple_lst_test "Make sure left white bishop cannot move" []
      (determine_piece_possible bishop83 (8, 3)
         (positions_from_board board8x8)
         8 8);
    make_tuple_lst_test "Move set for bishop at (3,2)"
      [ (4, 3); (4, 1) ]
      (determine_piece_possible bishop32 (3, 2)
         (positions_from_board bishoptestingboard)
         8 8);
    make_tuple_lst_test "Move set for bishop at (4,4)"
      [ (5, 5); (5, 3); (6, 2); (7, 1); (3, 5); (3, 3) ]
      (determine_piece_possible bishop44 (4, 4)
         (positions_from_board bishoptestingboard)
         8 8);
    make_tuple_lst_test "Move set for bishop at (5,4)"
      [ (6, 5); (7, 6); (6, 3); (7, 2); (4, 5); (3, 6); (4, 3) ]
      (determine_piece_possible bishop54 (5, 4)
         (positions_from_board bishoptestingboard)
         8 8);
    make_tuple_lst_test "Move set for bishop at (5,5)"
      [ (6, 6); (6, 4); (4, 6); (3, 7); (2, 8); (4, 4) ]
      (determine_piece_possible bishop55 (5, 5)
         (positions_from_board bishoptestingboard)
         8 8);
    (*TESTING QUEENS*)
    make_tuple_lst_test "Make sure left white queen cannot move" []
      (determine_piece_possible queen84 (8, 4)
         (positions_from_board board8x8)
         8 8);
    make_tuple_lst_test "Move set for queen at (3,1)"
      [
        (3, 2);
        (3, 3);
        (3, 4);
        (3, 5);
        (3, 6);
        (3, 7);
        (3, 8);
        (4, 1);
        (5, 1);
        (6, 1);
        (7, 1);
        (4, 2);
        (5, 3);
      ]
      (determine_piece_possible queen31 (3, 1)
         (positions_from_board queentestingboard)
         8 8);
    make_tuple_lst_test "Move set for queen at (5,3)"
      [
        (5, 4);
        (5, 2);
        (5, 1);
        (6, 3);
        (4, 3);
        (3, 3);
        (2, 3);
        (6, 4);
        (6, 2);
        (4, 4);
        (3, 5);
        (2, 6);
        (4, 2);
        (3, 1);
      ]
      (determine_piece_possible queen53 (5, 3)
         (positions_from_board queentestingboard)
         8 8);
    make_tuple_lst_test "Move set for queen at (5,5)"
      [
        (5, 6);
        (5, 7);
        (5, 8);
        (5, 4);
        (6, 5);
        (4, 5);
        (3, 5);
        (2, 5);
        (6, 6);
        (6, 4);
        (4, 6);
        (3, 7);
        (2, 8);
        (4, 4);
        (3, 3);
        (2, 2);
      ]
      (determine_piece_possible queen55 (5, 5)
         (positions_from_board queentestingboard)
         8 8);
  ]

let suite =
  "test suite for Chess game 123"
  >::: List.flatten [ board_tests; move_piece_tests ]

let _ = run_test_tt_main suite
