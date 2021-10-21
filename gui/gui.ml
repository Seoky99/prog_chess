open Raylib
open Raygui
open Chess
open Board

(*An object of type tile which contains a rectangle and a position*)
type tile = {
  rect : Rectangle.t;
  pos : Board.position
}
(*A row of tiles*)
type row = {
  tiles : tile list
}
(*The height of the window*)
let height = 1000
(*The width of the window*)
let width = 1000
let buttonrect : Rectangle.t = Rectangle.create (float_of_int (width / 2 - 250)) (float_of_int (height / 2)) 500. 150.
(*The rectangle representing the text box user input*)
let textrect : Rectangle.t = Rectangle.create (float_of_int (width / 2 - 150)) (float_of_int (height / 2 + 150)) 300. 150.
(*(Is_num_pressed () checks if a new number has been pressed*)
  let is_num_pressed () = 
    is_key_pressed Two ||
    is_key_pressed Three ||
    is_key_pressed Four ||
    is_key_pressed Five ||
    is_key_pressed Six ||
    is_key_pressed Seven ||
    is_key_pressed Eight ||
    is_key_pressed Nine
(*(Map json) Creates board from some JSON*)
let map (json : Yojson.Basic.t) : board = board_from_json json
(*(Get_input ()) Gets the val associated pressed char*)
let get_input () = 
  int_of_char (get_char_pressed ()) - 48
(*(Make_text_box () num) Creates the GUI text input box*)
let make_text_box () num=
  draw_rectangle_rec textrect Color.lightgray;
  draw_text "Input a number between 2 and 9" ((width / 2 - 170)) ((height / 2 + 300)) 20 Color.black;
  draw_text (string_of_int num) (width / 2 - 5) (height / 2 + 200) 40 Color.darkgray
(*(Make_even_row () num size os_x os_y) creates an even number row*)
let rec make_even_row () num size os_x os_y= 
  if num > 0 then (
    if num mod 2 = 0 then draw_rectangle os_x os_y size size Color.white 
    else if num mod 2 = 1 then draw_rectangle os_x os_y size size Color.black
    else ();
    make_even_row () (num - 1) size (os_x + size) os_y)
(*(Make_odd_row () num size os_x os_y) creates an odd number row*)
let rec make_odd_row () num size os_x os_y= 
  if num > 0 then (
    if num mod 2 = 0 then draw_rectangle os_x os_y size size Color.black 
    else if num mod 2 = 1 then draw_rectangle os_x os_y size size Color.white
    else ();
    make_odd_row () (num - 1) size (os_x + size) os_y)
(*(Make_grid () num size os_y) creates a chess board*)
let rec make_grid () num cur size os_y = 
  if cur > 0 then (
    if cur mod 2 = 0 then make_even_row () num size 0 os_y
    else if cur mod 2 = 1 then make_odd_row () num size 0 os_y
    else ();
    make_grid () num (cur - 1) size (os_y + size))

(*(Make_board () num) Creates the grid board and closes window if user exits*)
let rec make_board () num = 
  match window_should_close () with 
    | true -> close_window ()
    | false -> 
      begin_drawing ();
      clear_background Color.gray;
      make_grid () num num (1000 / num) 0;
      end_drawing ();
      make_board () num
(*(Setup ()) Creates the window with a fps of 60*)
let setup () =
  init_window height width "PROG CHESS";
  set_target_fps 60

(*(Menu_helper ()) is a helper for the main loop*)
let menu_helper () num = 
  make_text_box () (num);
  if button buttonrect "PLAY" then (
    make_board () (num))

(*(Loop () num) Loops the menu creating the map when user clicks start button*)
let rec loop () num =
    match window_should_close () with 
     | true -> close_window ()
     | false -> 
        begin_drawing ();
        clear_background Color.gray;
        draw_text "PROG CHESS" (width / 2 - 260) (height / 4) 80 Color.black;
        if is_num_pressed () then let num = get_input () in (
          menu_helper () num;
          end_drawing ();
          loop () num)
        else (
          menu_helper () num;
          end_drawing (); 
          loop () num)
(*Sets up the window then runs it until it should be closed*)
let () = loop (setup ()) 0