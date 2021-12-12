exception Empty

exception Malformed

type object_phrase = (int * int) list

type command =
  | Enter
  | Move of object_phrase
  | Quit
  | Invalid

let get_word_list str = String.split_on_char ' ' str

let rec get_words lst =
  match lst with
  | [] -> []
  | h :: lst -> if h = " " then get_words lst else h :: get_words lst

let get_first str = List.hd (get_words (get_word_list str))

let get_rest str = List.tl (get_words (get_word_list str))

let convert str =
  let lwr = String.lowercase_ascii str in

  let letter = lwr.[0] in
  let letter_number = int_of_char letter - 96 in
  let number = int_of_char lwr.[1] - 48 in
  (letter_number, number)

let convert_coord lst = List.map convert lst

let get_verb (str : string) =
  match String.lowercase_ascii str with
  | "enter" -> Enter
  | "move" -> Move []
  | "quit" -> Quit
  | _ -> Invalid

let command_creation str =
  try
    let verb = get_verb str in
    match verb with
    | Enter -> Enter
    | Quit -> Quit
    | Move _ -> Move (convert_coord (get_rest str))
    | Invalid -> raise Malformed
  with
  | Empty -> raise Empty
  | Malformed -> raise Malformed

let parse str = command_creation str
