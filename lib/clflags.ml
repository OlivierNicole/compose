let to_debug = ref []

let enable_debug s = to_debug := s :: !to_debug

let disable_debug s =
  to_debug := List.filter (fun s' -> not (String.equal s s')) !to_debug

let debug s = List.mem s !to_debug
