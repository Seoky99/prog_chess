(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Chess\n";
  print_endline "Enter here\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "just testing it works \n";
      ()

(* Execute the game engine. *)

let () = main ()
