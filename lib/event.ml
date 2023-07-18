open StdLabels
open Llama_midi

type t =
  { timestamp : int
  ; pitch : int [@compare Int.compare] [@equal Int.equal]
  ; duration : int
  ; velocity : int
  }
[@@deriving show, eq, ord]

let events_of_midi midi =
  let open Llama_midi in
  (* notes_on.(n) is either the timestamp when n started playing, or -1 *)
  let notes_on = Array.make 1024 (-1, 0) in
  let cur_time = ref 0 in
  List.filter_map midi ~f:(fun Event.{ delta_time; message } ->
         cur_time := !cur_time + delta_time;
         let open Channel_voice_message in
         match message with
         | Channel_voice_message { channel = _; message = Note_on { note; velocity }}
           when fst notes_on.(note) = -1 ->
             Fmt.pr "%d: Note_on %d@," !cur_time note;
             notes_on.(note) <- !cur_time, velocity;
             None
         | ( Channel_voice_message { channel = _; message = Note_off { note; velocity = _ }}
           | Channel_voice_message { channel = _; message = Note_on { note; velocity = 0 }})
           when fst notes_on.(note) >= 0 ->
             Fmt.pr "%d: Note off %d@," !cur_time note;
             let t0, velocity = notes_on.(note) in
             notes_on.(note) <- -1, 0;
             Some { timestamp = t0; pitch = note; duration = !cur_time - t0; velocity }
         | Channel_voice_message { channel = _; message = Note_on { note; _ } } ->
             (* Note already being played: ignore *)
             Fmt.pr "%d: Note_on %d (ignored)@," !cur_time note;
             None
         | Channel_voice_message { channel = _; message = Note_off _ } ->
             Fmt.pr "%d: Note_off (ignored)@," !cur_time;
             (* Note already off: ignore *)
             None
         | Channel_voice_message { channel = _; message =
           (Polyphonic_key_pressure _ | Control_change _ | Program_change _ |
           Channel_pressure _ | Pitch_wheel_change _) }
         | System_message _
         | Meta_event _ ->
             Fmt.pr "%d: (ignored event)@," !cur_time;
             (* Ignore all other events *)
             None)

let events_to_midi ~channel evs =
  let open Event in
  let open Channel_voice_message in
  List.concat_map evs ~f:(fun { timestamp; pitch; duration; velocity } ->
      let on =
        ( timestamp
        , Message.Channel_voice_message
            { channel
            ; message = Note_on { note = pitch; velocity } } )
      in
      let off =
        ( timestamp + duration
        , Message.Channel_voice_message
            { channel
            ; message = Note_off { note = pitch; velocity } } )
      in
      [on; off])
  |> List.sort ~cmp:(fun (ts, _) (ts', _) -> Int.compare ts ts')
  |> List.fold_left_map ~init:0 ~f:(fun last_timestamp (timestamp, message) ->
      timestamp, { delta_time = timestamp - last_timestamp; message })
  |> snd
