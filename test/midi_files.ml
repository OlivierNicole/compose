open StdLabels
open Cope

let file =
  try Sys.argv.(1) with Invalid_argument _ -> failwith "expects filename argument"

(* Read a MIDI file and write them to another file. The two files should be
   identical. *)
let () =
  let open Llama_midi in
  let reader = File_reader.of_path file in
  let data = File_reader.read reader in
  let path' = file ^ "2.mid" in
  let writer = File_writer.of_path path' in
  File_writer.write writer data;
  let reader' = File_reader.of_path path' in
  let data' = File_reader.read reader' in
  assert (Data.equal data data')
