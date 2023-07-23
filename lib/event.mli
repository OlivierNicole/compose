type t =
  { timestamp : int
  ; pitch : int  (** We use a pitch of 0 to represent rests. *)
  ; duration : int
  ; velocity : int
  }
[@@deriving show, eq, ord]
(** Represents a musical note. *)

val events_of_midi : ticks_per_beat:int -> Llama_midi.Track.t -> t list
(** Converts a track of MIDI events into a list of note events. Ignores channel
    information. Unlike in MIDI, silence is represented by explicit rest
    events, unless they last less than 3 % of a beat. *)

val events_to_midi : channel:int -> t list -> Llama_midi.Track.t
(** Converts a list of note events into a track of MIDI events on channel
    [channel]. *)
