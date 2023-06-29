open Mm_midi
open Cope

let file =
  try Sys.argv.(1) with Invalid_argument _ -> failwith "expects filename argument"

let () =
  let buffer = MIDI.Multitrack.create 2 100000 in
  let reader = new MIDI.IO.Reader.of_file file in
  Fmt.pr "@[<v>";
  Fmt.(pr "read = %d\n") @@ reader#read 480 buffer 0 100000;
  let data = MIDI.data buffer.(0) in
  Fmt.(pr "@[<v 1>data =@ %a@]" (list (pair int Print_Mm.MIDI.pp_event ~sep:comma)) data);
  Fmt.(cut stdout ());
  let events = Event.events_of_midi data in
  Fmt.(pr "@[<v 1>events =@ %a@]" (list Event.pp) events);
  Fmt.pr "@,@]"
