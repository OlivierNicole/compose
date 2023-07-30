val choose_one : 'a list -> Random.State.t -> 'a

val choose_weighted : (int * 'a) list -> Random.State.t -> 'a
