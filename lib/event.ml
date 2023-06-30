open StdLabels

type t =
  { timestamp : int
  ; pitch : Mm_audio.Audio.Note.t
        [@printer Extend_Mm.Audio.Note.pp] [@compare Int.compare] [@equal Int.equal]
  ; duration : int
  ; velocity : float
  }
[@@deriving show, eq, ord]

let events_of_midi midi =
  let open Mm_midi in
  (* notes_on.(n) is either the timestamp when n started playing, or -1 *)
  let notes_on = Array.make 1024 (-1, 0.) in
  List.rev
  @@ List.fold_left midi ~init:[] ~f:(fun evs (timestamp, midi_event) ->
         match midi_event with
         | MIDI.Note_on (note, velocity) when fst notes_on.(note) = -1 ->
             notes_on.(note) <- timestamp, velocity;
             evs
         | MIDI.Note_on _ ->
             (* Note already being played: ignore *)
             evs
         | MIDI.Note_off (note, _velocity) when fst notes_on.(note) >= 0 ->
             let t0, velocity = notes_on.(note) in
             notes_on.(note) <- -1, 0.;
             { timestamp = t0; pitch = note; duration = timestamp - t0; velocity } :: evs
         | MIDI.Note_off _ ->
             (* Note already off: ignore *)
             evs
         | _ ->
             (* Ignore all other events *)
             evs)

let events_to_midi evs =
  let open Mm_midi in
  List.fold_left evs ~init:[] ~f:(fun acc event ->
      let on = event.timestamp, MIDI.Note_on (event.pitch, event.velocity) in
      let off = event.timestamp + event.duration, MIDI.Note_off (event.pitch, 0.) in
      off :: on :: acc)
  |> List.rev
  |> List.sort ~cmp:(fun (ts, _) (ts', _) -> Int.compare ts ts')
