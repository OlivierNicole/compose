open StdLabels

type t = int list [@@deriving show, eq, ord]

let match_ i i' =
  assert (List.length i = List.length i');
  List.for_all2 i i' ~f:(fun n n' -> abs (n - n') <= 1)

let direction i = List.fold_left ~f:( + ) ~init:0 i
