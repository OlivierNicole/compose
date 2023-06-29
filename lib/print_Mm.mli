module Audio : sig
  module Note : sig
    type t = Mm_audio.Audio.Note.t [@@deriving show]
  end
end

module MIDI : sig
  type event = Mm_midi.MIDI.event [@@deriving show]
end
