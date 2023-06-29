type t =
  { timestamp : int
  ; pitch : Mm_audio.Audio.Note.t
  ; duration : int
  }
[@@deriving show]

val events_of_midi : (int * Mm_midi.MIDI.event) list -> t list
