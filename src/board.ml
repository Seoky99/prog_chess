open Yojson.Basic.Util 

(** Represents a single chess board square*)
type position = {
  id : int * int; 
  obstacle : string;
  color : string;
}

(** Keeps track of dimensions of board, and positions as 2D list. *)
type board = {
  number_of_rows : int;
  number_of_columns : int;
  positions : position list list;
}

(** Converts a yojson list to a pair 
Requires: the json list you pass in must have two elements 
Note: there must be a prettier way to write this but I forgot how to pattern match against lists*)
let yojsonlist_to_tuple = function 
  | x :: y :: [] -> (to_int x, to_int y)
  | _  -> failwith "Nonreal position"

(** [position_from_json json] is a representation of am individual square. *)
let position_from_json json = 
  {
      id = json |> member "id" |> to_list |> yojsonlist_to_tuple; 
      obstacle = json |> member "obstacle" |> to_string;
      color = json |> member "color" |> to_string;
  }

(** [positions1D json] is the list of all the squares on the board.
Example: 2x2 board will store positions w/ id [(1,1), (1,2), (2,1), (2,2)]*)
let positions1D json = 
  json |> member "positions" |> to_list |> List.map position_from_json 

(** [dimensionsTuple json] is the dimensions of the board (row, col)*)
let dimensionsTuple json = 
  json |> member "name" |> to_list |> yojsonlist_to_tuple

(** [positions2D pos_lst c x rowacc boardacc] helps construct the positions -
implemented as a 2D list comprised of position types*)
let rec positions2D pos_lst c x rowacc boardacc = 
  match pos_lst with 
  | [] ->  failwith "you moron. you imbecile. you screwed up bigtime."
  | h :: _ when x = c -> positions2D pos_lst c 1 [] ((h :: rowacc) :: boardacc)
  | h :: _ -> positions2D pos_lst c (x + 1) (h :: rowacc) (boardacc)

let board_from_json json = 
  {
    number_of_rows = fst (dimensionsTuple json); 
    number_of_columns = snd (dimensionsTuple json);
    positions = positions2D (positions1D json) (snd (dimensionsTuple json) ) 1 [] []
} 

(** TO DO: Implement nth row, nth column, finding positions
to test if abstraction is correct 
let n_row n board = 

let n_col n board = 

  *) 
  