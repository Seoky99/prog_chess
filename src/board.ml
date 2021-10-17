open Yojson.Basic.Util 
open Piece 

(** Represents where the chess board square is located*)
type id = (int * int) 

(** Represents a single chess board square*)
type position = {
  id : id; 
  obstacle : string;
  color : string;
  piece : piece 
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
      piece = json |> member "piece" |> to_string |> make_piece
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
let rec positions2D (pos_lst : position list) c x rowacc boardacc = 
  match pos_lst with 
  | [] ->  List.rev(boardacc) 
  | h :: t when x = c -> positions2D t c 1 [] ((List.rev(h :: rowacc)) :: boardacc)
  | h :: t -> positions2D t c (x + 1) (h :: rowacc) (boardacc)

let board_from_json json = 
  {
    number_of_rows = fst (dimensionsTuple json); 
    number_of_columns = snd (dimensionsTuple json);
    positions = positions2D (positions1D json) (snd (dimensionsTuple json)) 1 [] []
} 

(* [nth_elt n lst] is the nth element of the list (starting from index 1). 
Raises: exception if the list is empty*)
let rec nth_elt n lst = 
  match lst with 
  | [] -> failwith "fuck you"
  | h :: _ when n <= 1 -> h 
  | _ :: t ->  nth_elt (n-1) t 

(* [lst_of_nth_elt n lst acc] takes in a 2D list and returns a list of the nth 
element of each list within the 2D lists 
Example: 2 [[(1,2); (2,3); (3,4)]; [(3,6); (7,4); (9,9)] [] returns you
[((2,3); (7,4))]*)
let rec lst_of_nth_elt n lst acc =
  match lst with 
  | [] -> List.rev acc 
  | h :: t -> 
      let nth = nth_elt n h in 
      lst_of_nth_elt n t (nth :: acc)  

let n_row n board = 
  nth_elt n board.positions

let n_col n board = 
  lst_of_nth_elt n board.positions [] 
  
let id_pos_lst (pos_lst : position list) = 
  List.map (fun x -> x.id) pos_lst 

let id_board (board : board) = 
  List.map ( fun x -> id_pos_lst x) board.positions 

let num_rows (board : board) : int = 
  board.number_of_rows

let piece_pos_lst (pos_lst : position list) = 
    List.map (fun x -> get_name (x.piece)) pos_lst 

let piece_board board = 
  List.map (fun x -> piece_pos_lst x) board.positions

let num_cols (board : board) : int = 
  board.number_of_columns
(** Finding position to test if abstraction is correct *) 

(* [get_color_helper lst id ] is a helper function to the get_color function 
that returns the color at a specified id location on the board 
Requires: The id is a valid id on the given board.*)
let rec get_color_helper lst (id:id):string= match 
lst with
| []->failwith "Invalid id given"
|h::t-> if (h.id=id) then h.color else get_color_helper t (id)

(*[get_helper lst id f ] is a higher order function that takes a get_helper function and applies it 
Requires: the input id is a valid id on the baord*)
let get_helper lst (id:id) f :string = match lst with 
| []-> failwith "Invalid id given"
|h::t -> f (h::t) id 

(*[get_color board id lst] outputs the color of the board at the given id location
Requires:id is a valid id on the given board*)
let get_color (board:board) (id:id):string= 
match  (List.flatten board.positions) with 
| [] -> failwith "Invalid Id given"
| h::t -> if (h.id=id) then h.color else get_helper (t) id get_color_helper

(*[get_obstacle_helper lst id] is a helper function for the get_obstacles function
and output is the obstacle at the id location of the board. Requires: id is a 
valid id location on the given board*)
let rec get_obstacle_helper lst (id:id):string =match lst with 
| [] -> failwith "Invalid id given" 
| h::t -> if (h.id=id) then h.obstacle else get_obstacle_helper t id

(*[get_obstacle board id lst] outputs the obstacle at the specified id location 
on the board. Requires: id is a valid id location the given board.*)
let get_obstacle (board:board) (id:id):string =
  match (List.flatten board.positions) with 
  | [] -> failwith "Invalid id given"
  | h::t -> if (h.id=id) then h.obstacle else get_helper t id get_obstacle_helper

(** Finding position to test if abstraction is correct *) 
