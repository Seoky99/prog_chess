open Yojson.Basic.Util 
(** Translate json to a chess board. 
Two dimensional list for each of the positions. *)

type position = {
  id : int * int; 
  obstacle : string;
  color : string;
}

(* Keeping the # of rows and columns, as well 
how to access them general in case we want to implement 
piece functions more flexibly *)
type board = {
  number_of_rows : int;
  number_of_columns : int;
  positions : position list list;
}

(* there must be a prettier way to write this
but I forgot how to pattern match against lists*)
let convert_yojson_tuple = function 
  | x :: y :: [] -> (to_int x, to_int y)
  | _  -> failwith "Nonreal position"

let position_from_json json = 
  {
      id = json |> member "id" |> to_list |> convert_yojson_tuple; 
      obstacle = json |> member "obstacle" |> to_string;
      color = json |> member "color" |> to_string;
  }

let positions1D json = 
  json |> member "positions" |> to_list |> List.map position_from_json 

let dimensionsTuple json = 
  json |> member "name" |> to_list |> convert_yojson_tuple


let rec positions2D pos_lst c x acc1 acc2 = 
  match pos_lst with 
  | [] ->  failwith "you moron. you imbecile. you screwed up bigtime."
  | h :: _ when x = c -> positions2D pos_lst c 1 [] ((h :: acc1) :: acc2)
  | h :: _ -> positions2D pos_lst c (x + 1) (h :: acc1) (acc2)

let board_from_json json = 
  {
    number_of_rows = fst (dimensionsTuple json); 
    number_of_columns = snd (dimensionsTuple json);
    positions = positions2D (positions1D json) (snd (dimensionsTuple json) ) 1 [] []
} 

let testplz input = input + 2 
  