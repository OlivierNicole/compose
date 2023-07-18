open StdLabels
open Cope

let file =
  try Sys.argv.(1) with Invalid_argument _ -> failwith "expects filename argument"

(* Read MIDI events from a file, keep only the Note_{on,off} events, then check that
   writing them to another file and re-reading them is idempotent *)
let () =
  let open Llama_midi in
  Fmt.pr "@[<v>";
  let reader = File_reader.of_path file in
  let data = File_reader.read reader in
  Fmt.pr "%s@," @@ Data.to_string data;
  let path' = file ^ "2.mid" in
  let writer = File_writer.of_path path' in
  File_writer.write writer data;
  (*
  let reader' = File_reader.of_path path' in
  let data' = File_reader.read reader' in
  Fmt.pr "%s@," @@ Data.to_string data';
  *)
  Fmt.pr "@,@]"
