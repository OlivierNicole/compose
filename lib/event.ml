open StdLabels
open Llama_midi

let debug fmt =
  if Clflags.debug "events"
  then Stdlib.Format.(eprintf (fmt ^^ "%!"))
  else Stdlib.Format.(ifprintf err_formatter fmt)

type t =
  { timestamp : int
  ; pitch : int [@compare Int.compare] [@equal Int.equal]
  ; duration : int
  ; velocity : int
  }
[@@deriving show, eq, ord]

let events_of_midi ~ticks_per_beat midi =
  let open Llama_midi in
  (* notes_on.(n) is either the timestamp when n started playing, or -1 *)
  let notes_on = Array.make 1024 (-1, 0) in
  let cur_time = ref 0 in
  let time_last_rest = ref 0 in
  let duration_last_note = ref 0 in
  List.filter_map midi ~f:(fun Event.{ delta_time; message } ->
      cur_time := !cur_time + delta_time;
      let open Channel_voice_message in
      match message with
      | Channel_voice_message { channel = _; message = Note_on { note; velocity } }
        when fst notes_on.(note) = -1 ->
          debug "%d: Note_on %d\n" !cur_time note;
          let silent = Array.for_all ~f:(fun (ts_on, _) -> ts_on = -1) notes_on in
          notes_on.(note) <- !cur_time, velocity;
          (* If currently there is silence, emit an explicit rest *)
          if silent
          then
            let duration = !cur_time - !time_last_rest in
            (* Only emit a rest if it is longer than 5 % of last note or 5
               % of a beat. The reason is that some software (I'm looking at
               you, MuseScore) encodes notes as slightly shorter than their
               theoretical duration, probably to mimic real performance
               data.*)
            if float_of_int duration /. float_of_int !duration_last_note >= 0.1
               || float_of_int duration /. float_of_int ticks_per_beat >= 0.15
            then (
              debug "@[<h 2>emit rest at %d of %d@]\n" !time_last_rest duration;
              Some
                { timestamp = !time_last_rest
                ; pitch = 0
                ; duration = !cur_time - !time_last_rest
                ; velocity = 0
                })
            else (
              debug
                "@[<h 2>(ignored rest at %d lasting only %d ticks)"
                !time_last_rest
                duration;
              None)
          else None
      | Channel_voice_message { channel = _; message = Note_off { note; velocity = _ } }
      | Channel_voice_message { channel = _; message = Note_on { note; velocity = 0 } }
        when fst notes_on.(note) >= 0 ->
          debug "%d: Note off %d\n" !cur_time note;
          let t0, velocity = notes_on.(note) in
          notes_on.(note) <- -1, 0;
          let duration = !cur_time - t0 in
          duration_last_note := duration;
          (* If no more notes are played, record it *)
          if Array.for_all ~f:(fun (ts_on, _) -> ts_on = -1) notes_on
          then (
            debug "@[<h 2>prepare rest@]\n";
            time_last_rest := !cur_time);
          Some { timestamp = t0; pitch = note; duration; velocity }
      | Channel_voice_message { channel = _; message = Note_on { note; _ } } ->
          (* Note already being played: ignore *)
          debug "%d: Note_on %d (ignored)\n" !cur_time note;
          None
      | Channel_voice_message { channel = _; message = Note_off _ } ->
          debug "%d: Note_off (ignored)\n" !cur_time;
          (* Note already off: ignore *)
          None
      | Channel_voice_message
          { channel = _
          ; message =
              ( Polyphonic_key_pressure _
              | Control_change _
              | Program_change _
              | Channel_pressure _
              | Pitch_wheel_change _ )
          }
      | System_message _ | Meta_event _ ->
          debug "%d: (ignored event)\n" !cur_time;
          (* Ignore all other events *)
          None)

let events_to_midi ~channel evs =
  let open Event in
  let open Channel_voice_message in
  List.concat_map evs ~f:(fun { timestamp; pitch; duration; velocity } ->
      (* Ignore rests *)
      if pitch = 0
      then []
      else
        let on =
          ( timestamp
          , Message.Channel_voice_message
              { channel; message = Note_on { note = pitch; velocity } } )
        in
        let off =
          ( timestamp + duration
          , Message.Channel_voice_message
              { channel; message = Note_off { note = pitch; velocity } } )
        in
        [ on; off ])
  |> List.sort ~cmp:(fun (ts, _) (ts', _) -> Int.compare ts ts')
  |> List.fold_left_map ~init:0 ~f:(fun last_timestamp (timestamp, message) ->
         timestamp, { delta_time = timestamp - last_timestamp; message })
  |> snd
  |> fun track ->
  track
  @ [ Event.{ delta_time = 0; message = Message.Meta_event Meta_event.End_of_track } ]
