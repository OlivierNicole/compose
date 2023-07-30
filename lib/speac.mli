type t =
  | S
  | P
  | E
  | A
  | C
[@@deriving show, eq, ord]

val generate : rough_phrase_length:int -> Random.State.t -> t list
