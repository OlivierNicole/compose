open StdLabels

module Audio = struct
  module Note = struct
    type t =
      (Mm_audio.Audio.Note.t
      [@printer fun fmt n -> Format.pp_print_string fmt (Mm_audio.Audio.Note.to_string n)]
      [@equal Int.equal]
      [@compare Int.compare])
    [@@deriving show, eq, ord]
  end
end

module MIDI = struct
  type event =
    [%import: (Mm_midi.MIDI.event[@with Mm_audio.Audio.Note.t := Audio.Note.t])]
  [@@deriving show, eq, ord]

  type timed_event = int * event [@@deriving show, eq, ord]

  type division = [%import: Mm_midi.MIDI.division] [@@deriving show, eq, ord]

  module IO = struct
    let all_events_of_file ~path ~sample_rate =
      let open Mm_midi.MIDI in
      let chunk_length = 10 (* XXX *) * sample_rate in
      let reader = new IO.Reader.of_file path in
      let div = reader#division in
      let buffer = ref (Multitrack.create 2 chunk_length) in
      let max_length = ref chunk_length in
      let ofs = ref 0 in
      let read_length = ref 0 in
      while
        read_length := reader#read sample_rate !buffer !ofs chunk_length;
        !read_length = chunk_length
      do
        ofs := !ofs + chunk_length;
        max_length := !max_length * 2;
        let new_buf = Multitrack.create 2 !max_length in
        blit_all !buffer.(0) new_buf.(0);
        blit_all !buffer.(1) new_buf.(1);
        Multitrack.clear !buffer 0 (Multitrack.duration !buffer);
        Multitrack.clear !buffer 1 (Multitrack.duration !buffer);
        buffer := new_buf
      done;
      reader#close;
      merge !buffer.(0) !buffer.(1);
      div, !ofs + !read_length, [| data !buffer.(0); |]

    let events_to_file ~path ~sample_rate evs =
      let open Mm_midi.MIDI.IO in
      let writer = new Writer.to_file sample_rate path in
      let curdelta = ref 0 in
      List.iter
        ~f:(fun (ts, ev) ->
          (* Apparently [advance] takes an absolute timestamp? Nonsensical *)
          writer#advance (ts - !curdelta);
          curdelta := ts;
          writer#put 0 ev)
        evs;
      writer#close
  end
end
