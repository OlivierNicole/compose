module Audio = struct
  module Note = struct
    type t =
      (Mm_audio.Audio.Note.t
      [@printer fun fmt n -> Format.pp_print_string fmt (Mm_audio.Audio.Note.to_string n)])
    [@@deriving show]
  end
end

module MIDI = struct
  type event =
    [%import: (Mm_midi.MIDI.event[@with Mm_audio.Audio.Note.t := Audio.Note.t])]
  [@@deriving show]
end
