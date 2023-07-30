type t =
  | S
  | P
  | E
  | A
  | C
[@@deriving show, eq, ord]

(* What can follow what. *)
let syntax = function
  | S -> [ P; E; A ]
  | P -> [ S; A; C ]
  | E -> [ S; P; A; C ]
  | A -> [ E; C ]
  | C -> [ S; P; E; A ]

let choose_one l rand = List.nth l @@ Random.State.int rand @@ List.length l

let generate ~rough_phrase_length rand =
  let[@tail_mod_cons] rec aux first countdown =
    if countdown <= 0
    then []
    else
      let new_ = choose_one (syntax first) rand in
      let countdown = if equal new_ A then countdown - 1 else countdown in
      new_ :: aux new_ countdown
  in
  aux (choose_one [ S; P; E; A; C ] rand) rough_phrase_length
