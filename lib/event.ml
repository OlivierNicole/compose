open StdLabels

type t =
  { timestamp : int
  ; pitch : Mm_audio.Audio.Note.t [@printer Print_Mm.Audio.Note.pp]
  ; duration : int
  }
[@@deriving show]

let events_of_midi midi =
  let open Mm_midi in
  (* notes_on.(n) is either the timestamp when n started playing, or -1 *)
  let notes_on = Array.make 1024 (-1) in
  List.rev
  @@ List.fold_left midi ~init:[] ~f:(fun evs (timestamp, midi_event) ->
         match midi_event with
         | MIDI.Note_on (note, _velocity) when notes_on.(note) = -1 ->
             notes_on.(note) <- timestamp;
             evs
         | MIDI.Note_on _ ->
             (* Note already being played: ignore *)
             evs
         | MIDI.Note_off (note, _velocity) when notes_on.(note) >= 0 ->
             let t0 = notes_on.(note) in
             notes_on.(note) <- -1;
             { timestamp = t0; pitch = note; duration = timestamp - t0 } :: evs
         | MIDI.Note_off _ ->
             (* Note already off: ignore *)
             evs
         | _ ->
             (* Ignore all other events *)
             evs)
