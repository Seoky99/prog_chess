exception Malformed

exception Empty

type object_phrase = (int * int) list

type command =
  | Move of object_phrase
  | HenryIsABitch

val parse : string -> command
