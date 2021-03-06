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
      draw_rectangle os_x os_y size size Color.black
    else ();
    make_even_row () (num - 1) size (os_x + size) os_y)

(*(Make_odd_row () num size os_x os_y) creates an odd number row*)
let rec make_odd_row () num size os_x os_y =
  if num > 0 then (
    if num mod 2 = 0 then draw_rectangle os_x os_y size size Color.black
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

let render_images () (height : int) (width : int) =
  draw_texture
    (load_texture_from_image white_knight_image)
    (width / 3) (height / 3) Color.white;
  draw_texture
    (load_texture_from_image black_pawn_image)
    (width * 2) (height * 2) Color.white

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
      render_images () (1000 / num1) (1000 / num2);
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