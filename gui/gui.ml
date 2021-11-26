open Raylib
open Raygui
open Chess
open Board
open Loaded_images

(** This code is temporarility here which allows the code to be built
    and rendered.*)
let image_pawn = black_pawn_image

(*Allow for hexagons, squares, rectangles BUT min width is reg. chess
  board*)

(*An object of type tile which contains a rectangle and a position*)
type tile = {
  rect : Rectangle.t;
  pos : Board.position;
}

(*A row of tiles*)
type row = { tiles : tile list }

(*The height of the window*)
let height = 1000

(*The width of the window*)
let width = 1000

let buttonrect : Rectangle.t =
  Rectangle.create
    (float_of_int ((width / 2) - 250))
    (float_of_int ((height / 2) - 50))
    500. 150.

(*The rectangle representing the text box user input*)
let textrect1 : Rectangle.t =
  Rectangle.create
    (float_of_int ((width / 2) - 150))
    (float_of_int ((height / 2) + 150))
    300. 150.

(*The rectangle representing the text box user input*)
let textrect2 : Rectangle.t =
  Rectangle.create
    (float_of_int ((width / 2) - 150))
    (float_of_int ((height / 2) + 325))
    300. 150.

(*The rectangle representing the text box user input*)
let textrect : Rectangle.t =
  Rectangle.create
    (float_of_int ((width / 2) - 150))
    (float_of_int ((height / 2) + 150))
    300. 150.

(*(Is_num_pressed () checks if a new number has been pressed*)
let is_num_pressed () =
  let key = get_key_pressed () in
  if
    key = Zero || key = One || key = Two || key = Three || key = Four
    || key = Five || key = Six || key = Seven || key = Eight
    || key = Nine
  then true
  else false

(*Checks whether the mouse is over the textbox to add a value*)
let can_add_num_col () pt str =
  if Raylib.check_collision_point_rec pt textrect1 then
    if is_num_pressed () then
      if String.length str < 2 then true else false
    else false
  else false

(*Checks whether the mouse is over the textbox to add a value*)
let can_add_num_row () pt str =
  if Raylib.check_collision_point_rec pt textrect2 then
    if is_num_pressed () then
      if String.length str < 2 then true else false
    else false
  else false

(*(Map json) Creates board from some JSON*)
let map (json : Yojson.Basic.t) : board = board_from_json json

(*(Get_input ()) Gets the val associated pressed char*)
let get_input () = int_of_char (get_char_pressed ()) - 48

(*(Make_text_box () num) Creates the GUI text input box*)
let make_text_box () num =
  draw_rectangle_rec textrect Color.lightgray;
  draw_text "Input a number between 2 and 9"
    ((width / 2) - 170)
    ((height / 2) + 300)
    20 Color.black;
  draw_text (string_of_int num)
    ((width / 2) - 5)
    ((height / 2) + 200)
    40 Color.darkgray

(*(Make_even_row () num size os_x os_y) creates an even number row*)
let rec make_even_row () num size os_x os_y =
  if num > 0 then (
    if num mod 2 = 0 then draw_rectangle os_x os_y size size Color.white
    else if num mod 2 = 1 then
      draw_rectangle os_x os_y size size Color.green
    else ();
    make_even_row () (num - 1) size (os_x + size) os_y)

(*(Make_odd_row () num size os_x os_y) creates an odd number row*)
let rec make_odd_row () num size os_x os_y =
  if num > 0 then (
    if num mod 2 = 0 then draw_rectangle os_x os_y size size Color.green
    else if num mod 2 = 1 then
      draw_rectangle os_x os_y size size Color.white
    else ();
    make_odd_row () (num - 1) size (os_x + size) os_y)

(*(Make_grid () num size os_y) creates a chess board*)
let rec make_grid () num1 num2 cur size os_y =
  if cur > 0 then (
    if cur mod 2 = 0 then make_even_row () num2 size 0 os_y
    else if cur mod 2 = 1 then make_odd_row () num2 size 0 os_y
    else ();
    make_grid () num1 num2 (cur - 1) size (os_y + size))

let center_of_grid_box_x (x1 : int) (x2 : int) =
  (float_of_int x1 +. float_of_int x2) /. 2.0

let center_of_grid_box_y (y1 : int) (y2 : int) =
  (float_of_int y1 +. float_of_int y2) /. 2.0

let put_piece_on_board_helper_row
    (piece_row : int)
    (width : int)
    (num_rows : int) =
  let box_width = width / num_rows in
  let end_of_the_box_x_coord = piece_row * box_width in
  let beg_of_the_box_x_coord = (piece_row - 1) * box_width in
  int_of_float
    (center_of_grid_box_x beg_of_the_box_x_coord end_of_the_box_x_coord)

let put_piece_on_board_helper_col
    (piece_col : int)
    (height : int)
    (num_cols : int) =
  let box_width = height / num_cols in
  let given_piece_location = piece_col * box_width in
  given_piece_location

(*[top_left_corner_x x1 x2 width num_cols] outputs the left coordinate x
  for the box which is denoted by x1*)
let top_left_corner_x
    (x1 : int)
    (x2 : int)
    (width : int)
    (num_cols : int) =
  let x2 = x2 - x2 in
  let top_left_x = x1 + x2 in
  float_of_int (top_left_x - 1)
  *. (float_of_int width /. float_of_int num_cols)

(*[top_left_corner_y]Outputs the top left y coordinate of the box which
  is denoted by y1*)
let top_left_corner_y
    (y1 : int)
    (y2 : int)
    (height : int)
    (num_rows : int) =
  let y2 = y2 - y2 in
  let top_left_y = y1 + y2 in
  float_of_int (top_left_y - 1)
  *. (float_of_int height /. float_of_int num_rows)

(*[piece_to_image p]This method gives as ouput the correct image given
  the piece name and the team of the piece*)
let piece_to_image p =
  let p_team = Piece.team_of p in
  let type_of_piece = Piece.get_name p in
  match type_of_piece with
  | "black_pawn" -> black_pawn_image
  | "white_pawn" -> white_pawn_image
  | "rook" ->
      if p_team = "black" then black_rook_image else white_rook_image
  | "knight" ->
      if p_team = "black" then black_knight_image
      else white_knight_image
  | "bishop" ->
      if p_team = "black" then black_bishop_image
      else white_bishop_image
  | "queen" ->
      if p_team = "black" then black_queen_image else white_queen_image
  | "king" ->
      if p_team = "black" then black_king_image else white_king_image
  | _ -> error_image

(*[render_images_helper p width height num_rows num_cols]This method is
  a helper function for the render_images function. This method gives us
  how large the image should be for a given board and the appropriate
  image to render*)
let render_images_helper (p : position) width height num_rows num_cols =
  let id_location = Board.id_from_position p in
  let current_piece = Board.get_piece p in
  let image_for_piece = piece_to_image current_piece in
  let image_multiplier =
    match num_rows with
    | 8
    | 9 ->
        2.0
    | 10 -> 1.60
    | 11 -> 1.45
    | 12 -> 1.25
    | 13
    | 14 ->
        1.10
    | 15
    | 16 ->
        1.0
    | 17
    | 18 ->
        0.8
    | 19
    | 20 ->
        0.7
    | 21
    | 22 ->
        0.6
    | 23 -> 0.5
    | 24
    | 25 ->
        0.45
    | _ -> failwith "Invalid input"
  in
  if image_for_piece = error_image then ()
  else
    draw_texture_pro
      (load_texture_from_image image_for_piece)
      (Rectangle.create 0.0 0.0
         (float_of_int (Image.width image_for_piece))
         (float_of_int (Image.height image_for_piece)))
      (Rectangle.create
         (top_left_corner_x (snd id_location)
            (fst id_location + 1)
            width num_cols)
         (top_left_corner_y (fst id_location)
            (snd id_location + 1)
            height num_rows)
         (float_of_int (Image.width image_for_piece) *. image_multiplier)
         (float_of_int (Image.height image_for_piece)
         *. image_multiplier))
      (Vector2.create 1.0 1.0)
      0.0 Color.white

(*[render_images board width height num_rows num_cols]Renders all the
  pieces on the board when the game is started.*)
let render_images () cur_board width height num_rows num_cols =
  let position_list =
    List.flatten (Board.positions_from_board cur_board)
  in
  let rec render p_list =
    match p_list with
    | [] -> ()
    | h :: t ->
        render_images_helper h width height num_rows num_cols;
        render t
  in
  render position_list

(*(Make_board () num) Creates the grid board and closes window if user
  exits*)
let rec make_board () num1 num2 =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      begin_drawing ();
      clear_background Color.gray;
      if num1 <= num2 then make_grid () num1 num2 num1 (1000 / num2) 0
      else make_grid () num1 num2 num1 (1000 / num1) 0;
      render_images ()
        (Board.create_board num1 num2)
        1000 1000 num1 num2;
      end_drawing ();
      make_board () num1 num2

let rec getInt str =
  match str with
  | "" -> 0
  | _ ->
      if String.length str = 2 then
        (10 * (int_of_char (String.get str 0) - 48))
        + getInt (String.sub str 1 1)
      else int_of_char (String.get str 0) - 48

(*(Setup ()) Creates the window with a fps of 60*)
let setup () =
  init_window height width "PROG CHESS";
  set_target_fps 60

(*(Menu_helper ()) is a helper for the main loop*)
let menu_helper () num1 num2 =
  if button buttonrect "PLAY" then (
    if num1 <= num2 then set_window_size 1000 (num1 * (1000 / num2))
    else set_window_size (num2 * (1000 / num1)) 1000;
    make_board () num1 num2)

(*(Num_pressed_helper () num) creates the board if a num has been
  entered*)
let num_pressed_helper () num1 num2 =
  end_drawing ();
  print_int num1;
  print_int num2;
  menu_helper () num1 num2

(*(Loop () num) Loops the menu creating the map when user clicks start
  button*)
let rec loop () colVal rowVal =
  draw_text "PROG CHESS" ((width / 2) - 260) (height / 4) 80 Color.black;
  match window_should_close () with
  | true -> close_window ()
  | false ->
      begin_drawing ();
      clear_background Color.gray;
      draw_rectangle_rec textrect1 Color.lightgray;
      draw_rectangle_rec textrect2 Color.lightgray;
      draw_text
        "Hover over this box and input a row number between 8 and 25"
        ((width / 2) - 310)
        ((height / 2) + 300)
        20 Color.black;
      draw_text
        "Hover over this box and input a column number between 5 and 25"
        ((width / 2) - 320)
        ((height / 2) + 125)
        20 Color.black;
      if can_add_num_col () (get_mouse_position ()) colVal then (
        let num = string_of_int (get_input ()) in
        draw_text (colVal ^ num)
          ((width / 2) - 10)
          ((height / 2) + 200)
          40 Color.darkgray;
        draw_text rowVal
          ((width / 2) - 10)
          ((height / 2) + 375)
          40 Color.darkgray;
        num_pressed_helper () (getInt colVal) (getInt rowVal);
        loop () (colVal ^ num) rowVal);
      if can_add_num_row () (get_mouse_position ()) rowVal then (
        let num = string_of_int (get_input ()) in
        draw_text (rowVal ^ num)
          ((width / 2) - 10)
          ((height / 2) + 375)
          40 Color.darkgray;
        draw_text colVal
          ((width / 2) - 10)
          ((height / 2) + 200)
          40 Color.darkgray;
        num_pressed_helper () (getInt colVal) (getInt rowVal);
        loop () colVal (rowVal ^ num));
      draw_text colVal
        ((width / 2) - 10)
        ((height / 2) + 200)
        40 Color.darkgray;
      draw_text rowVal
        ((width / 2) - 10)
        ((height / 2) + 375)
        40 Color.darkgray;
      num_pressed_helper () (getInt colVal) (getInt rowVal);
      loop () colVal rowVal

(*Sets up the window then runs it until it should be closed*)
let () = loop (setup ()) "" ""