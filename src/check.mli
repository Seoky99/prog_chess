val check : Board.position list list -> string -> int -> int -> bool
(** [check pos_lst team num_cols num_rows] returns True if [team] is
    under check based on [pos_lst], and False if [team] is not under
    check.*)
