type t =
  { timestamp : int
  ; pitch : Mm_audio.Audio.Note.t
  ; duration : int
  ; velocity : float
  }
[@@deriving show, eq, ord]

val events_of_midi : (int * Mm_midi.MIDI.event) list -> t list

val events_to_midi : t list -> (int * Mm_midi.MIDI.event) list
