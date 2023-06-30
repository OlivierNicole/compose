open Mm_midi
open Cope
open StdLabels

let file =
  try Sys.argv.(1) with Invalid_argument _ -> failwith "expects filename argument"

let sample_rate =
  try int_of_string Sys.argv.(2)
  with Invalid_argument _ -> failwith "expects sample rate argument"

(* Read MIDI events from a file, keep only the Note_{on,off} events, then check that
   writing them to another file and re-reading them is idempotent *)
(* This actually doesn't work because mm does its own cooking with sample rates. *)
let () =
  Fmt.pr "@[<v>";
  let division, duration, data =
    Extend_Mm.MIDI.IO.all_events_of_file ~path:file ~sample_rate
  in
  Fmt.pr "division = %a@," Extend_Mm.MIDI.pp_division division;
  Fmt.pr "duration = %d@," duration;
  Fmt.(pr "@[<v 1>data =@ %a@]@," (list Extend_Mm.MIDI.pp_timed_event) data);
  let data =
    List.filter
      ~f:(function
        | _, MIDI.(Note_off _ | Note_on _) -> true
        | _ -> false)
      data
  in
  let other_file = file ^ ".2.mid" in
  if Sys.file_exists other_file
  then failwith ("Cannot write to " ^ other_file ^ ": file exists");
  Extend_Mm.MIDI.IO.events_to_file ~path:other_file ~sample_rate data;
  let division', duration', data' =
    Extend_Mm.MIDI.IO.all_events_of_file ~path:other_file ~sample_rate
  in
  Fmt.pr "division' = %a@," Extend_Mm.MIDI.pp_division division';
  Fmt.pr "duration' = %d@," duration';
  Fmt.(pr "@[<v 1>data' =@ %a@]@," (list Extend_Mm.MIDI.pp_timed_event) data');
  assert (duration = duration');
  assert (List.equal ~eq:Extend_Mm.MIDI.equal_timed_event data data');
  Sys.remove other_file;
  Fmt.pr "@,@]"
