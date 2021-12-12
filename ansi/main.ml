open Chess.Gamestate
open Chess.Comm

let rec file_to_string
    (init : string)
    (f : in_channel)
    (file_name : string) =
  try
    let line : string = init ^ "\n" ^ Stdlib.input_line f in
    file_to_string line f file_name
  with
  | End_of_file -> init

let print_file directory file_name : unit =
  let file_path = directory ^ Filename.dir_sep ^ file_name in
  let file = Stdlib.open_in file_path in
  print_string (file_to_string "" file file_name)

let convert_coord_to_id coord1 coord2 = 
  


(*CHANGE LATER TO CORRECT HEIGHT AND WIDTH*)
let current_state = start_game 8 8

type turn =
  | White
  | Black

(*Turn should be current_turn*)
let run_game st turn =
  let turn_str =
    match turn with
    | White -> "white"
    | Black -> "black"
  in

  (*CHANGE LATER TO CORRECT HEIGHT AND WIDTH*)
  let play_state = get_play_state st turn_str in

  let () =
    ANSITerminal.print_string [ ANSITerminal.green ] "Move a\n  piece!"
  in

  match play_state with
  | Stalemate ->
      failwith "TODO, open the stalemate window based on turn"
  | Checkmate -> failwith "TODO, open checkmate window based on turn"
  | Normal -> (
      (*PSEUDOCODE, substitue w/ real later*)
      match read_line () with
      | exception End_of_file -> ()
      | x -> let command = Comm.parse x in 
      match command with 
      | Move y -> BITCH
      | HenryIsABitch -> 
      
      )

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  (*print_file "/mnt/c/Users/Alexander/Downloads/chess" "menu.txt";*)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Chess\n";
  print_string "\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "just testing it\n\n  works \n";
      ();
      main ()

(* Execute the game engine. *)

let () = main ()
