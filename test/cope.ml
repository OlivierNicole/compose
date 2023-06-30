(*open Mm_midi*)
open Cope
open StdLabels

let file =
  try Sys.argv.(1) with Invalid_argument _ -> failwith "expects filename argument"

let sample_rate =
  try int_of_string Sys.argv.(2)
  with Invalid_argument _ -> failwith "expects sample rate argument"

let () =
  Fmt.pr "@[<v>";
  let _, duration, datas = Extend_Mm.MIDI.IO.all_events_of_file ~path:file ~sample_rate in
  Fmt.pr "duration = %d@," duration;
  (*
  Fmt.(pr "@[<v 1>data =@ %a@]" (list (pair int Extend_Mm.MIDI.pp_event ~sep:comma)) data);
  *)
  Fmt.(cut stdout ());
  let events = Array.map ~f:Event.events_of_midi datas in
  Array.iter ~f:(fun evs -> Fmt.(pr "@[<v 1>events =@ %a@]" (list Event.pp) evs)) events;
  Array.iteri events ~f:(fun i evs ->
        let out = open_out_bin Fmt.(str "marshalled_events%d" i) in
        Marshal.to_channel out evs [];
        close_out out;);
  (*
  let data' = Event.events_to_midi events in
  assert (
    List.equal
      ~eq:Extend_Mm.MIDI.equal_timed_event
      (List.filter
         ~f:(function
           | _, MIDI.(Note_off _ | Note_on _) -> true
           | _ -> false)
         data)
      data');
  Extend_Mm.MIDI.IO.events_to_file ~path:(file ^ ".2.mid") ~sample_rate data';
  *)
  Fmt.pr "@,@]"
