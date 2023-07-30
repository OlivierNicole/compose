type t = int list [@@deriving show, eq, ord]
(** A sequence of musical intervals, represented as a positive or negative
    number of half-steps. *)

(* Two motives match if each of their intervals are pairwise identical or
   differ only by one half-step. *)
val match_ : t -> t -> bool

val direction : t -> int
(** The direction of a sequence of intervals is the sum of those intervals,
    which is also the algebraic distance between the starting and ending notes. *)
