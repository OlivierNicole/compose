open Cope
open Llama_midi
open StdLabels

let input_files = ref []

let () =
  Arg.parse
    []
    (fun filename -> input_files := !input_files @ [filename])
    "cope <file1>";
  let work1 = match !input_files with
    | [f] -> f
    | _ -> (Fmt.epr "error: cope expects exactly 1 argument"; exit 1)
  in
  let reader = File_reader.of_path work1 in
  let work1_data = File_reader.read reader in
  Fmt.pr "@[<v>";
  (*
  Fmt.(pr "@[<v 2>data =@ %s@]" @@ Data.to_string data);
  Fmt.pr "@,";
  *)
  let event_lists = List.map ~f:Cope.Event.events_of_midi data.Data.tracks in
  List.iteri
    ~f:(fun i evs -> Fmt.(pr "@[<v 1>events track %d =@ %a@]"
    i
    (list Cope.Event.pp) evs))
    event_lists;
  Fmt.pr "@,@]"
