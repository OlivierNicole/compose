open StdLabels

let choose_one l rand = List.nth l @@ Random.State.int rand @@ List.length l

let choose_weighted l rand =
  let total = List.(fold_left ~f:(+) ~init:0 @@ map ~f:fst l) in
  let n = Random.State.bits rand in
  let scale x = Float.(to_int @@ (of_int x) /. (of_int total) *. of_int (1 lsl 30)) in
  let rec aux current = function
    | (c, x) :: rest ->
        if scale current <= n && n < scale (current + c) then x
        else aux (current + c) rest
    | [] -> assert false
  in
  aux 0 l
