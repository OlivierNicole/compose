(** Represents a musical note. *)
type t =
  { timestamp : int
  ; pitch : int
  ; duration : int
  ; velocity : int
  }
[@@deriving show, eq, ord]

(** Converts a track of MIDI events into a list of note events. Ignores channel
    information. *)
val events_of_midi : Llama_midi.Track.t -> t list

(** Converts a list of note events into a track of MIDI events on channel
    [channel]. *)
val events_to_midi : channel:int -> t list -> Llama_midi.Track.t
