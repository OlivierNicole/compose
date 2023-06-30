module Audio : sig
  module Note : sig
    type t = Mm_audio.Audio.Note.t [@@deriving show, eq, ord]
  end
end

module MIDI : sig
  type event = Mm_midi.MIDI.event [@@deriving show, eq, ord]

  type timed_event = int * event [@@deriving show, eq, ord]

  type division = Mm_midi.MIDI.division [@@deriving show, eq, ord]

  module IO : sig
    val all_events_of_file :
      path:string -> sample_rate:int -> division * int * ((int * event) list) array
    (** Reads channel 0 from a MIDI file. Returns the total duration (in "ticks"), the
          division and the list of events. *)

    val events_to_file : path:string -> sample_rate:int -> (int * event) list -> unit
    (** Writes to a single-channel MIDI file. Assumes that the list is sorted by
          increasing timestamps. *)
  end
end
