open Cope
open StdLabels

let input_files = ref []

let output_file = ref ""

(* Size of motives considered when searching for signatures *)
let motive_size = 4

(* Length of the output, in SPEAC phrases *)
let rough_phrase_length = 6

module List : sig
  include module type of List

  val group_by : ?keep_smaller_tail:bool -> int -> 'a list -> 'a list list

  val last : 'a list -> 'a

  val take : int -> 'a list -> 'a list

  val drop : int -> 'a list -> 'a list [@@warning "-32"]

  val split_at : int -> 'a list -> 'a list * 'a list

  val take_while : pred:('a -> bool) -> 'a list -> 'a list
end = struct
  include List

  let group_by ?(keep_smaller_tail = true) n l =
    let[@tail_mod_cons] rec group_by' cur_n acc n = function
      | x :: xs when cur_n > 0 -> group_by' (cur_n - 1) (x :: acc) n xs
      | _ :: _ as l (* n <= 0 *) -> List.rev acc :: group_by' n [] n l
      | [] when cur_n > 0 -> if keep_smaller_tail then [ List.rev acc ] else []
      | [] -> [ List.rev acc ]
    in
    group_by' n [] n l

  let rec last = function
    | [] -> raise (Invalid_argument "List.last")
    | [ x ] -> x
    | _ :: xs -> last xs

  let[@tail_mod_cons] rec take n = function
    | [] when n > 0 -> raise (Invalid_argument "List.take")
    | x :: xs when n > 0 -> x :: take (n - 1) xs
    | _ -> []

  let[@tail_mod_cons] rec drop n = function
    | [] when n > 0 -> raise (Invalid_argument "List.drop")
    | _ :: xs when n > 0 -> drop (n - 1) xs
    | l -> l

  let split_at n l =
    let rec aux acc n = function
      | [] when n > 0 -> raise (Invalid_argument "List.split_at")
      | x :: xs when n > 0 -> aux (x :: acc) (n - 1) xs
      | l (* n <= 0 *) -> List.rev acc, l
    in
    aux [] n l

  let[@tail_mod_cons] rec take_while ~pred = function
    | [] -> []
    | x :: xs -> if pred x then x :: take_while ~pred xs else []
end

(* generic_intervals [x0; x1; x2; ...] f is [f x0 x1; f x1 x2; ...]. *)
let rec generic_intervals ~f = function
  | [] | [ _ ] -> raise (Invalid_argument "intervals")
  | [ x; y ] -> [ f x y ]
  | x :: (y :: _ as rest) -> f x y :: generic_intervals ~f rest
  [@@tail_mod_cons]

let intervals = generic_intervals ~f:(fun x y -> y - x)

let duration_pattern = generic_intervals ~f:(fun x y -> float_of_int x /. float_of_int y)

let debug fmt =
  if Clflags.debug "main"
  then Stdlib.Format.(eprintf (fmt ^^ "%!"))
  else Stdlib.Format.(ifprintf err_formatter fmt)

let read_first_track path =
  let reader = Llama_midi.File_reader.of_path path in
  let data = Llama_midi.File_reader.read reader in
  let ticks_per_beat =
    let open Llama_midi in
    match data.Data.header.Header.division with
    | Ticks_per_quarter_note x -> x
    | Time_code _ -> failwith "SMPTE format not supported"
  in
  debug "@[<v 2>data =@ %s@]\n" @@ Llama_midi.Data.to_string data;
  let voice =
    Event.events_of_midi ~ticks_per_beat (List.hd data.Llama_midi.Data.tracks)
  in
  Fmt.(debug "@[<v 1>work 1 upper voice =@ %a@]\n" (list Event.pp) voice);
  `Raw_data data, voice, `Ticks_per_beat ticks_per_beat

(* Compute note durations based on the time delta with the next note (or rest),
   rather than the duration encoded in MIDI. The reason is that some software
   (I'm looking at you, MuseScore) encodes notes as slightly shorter than their
   theoretical duration, probably to mimic real performance data. Of course, we
   can't do that for the last note. *)
let _actual_note_durations = function
  | [] -> raise (Invalid_argument "actual_note_durations: unexpected empty list")
  | notes ->
      generic_intervals ~f:(fun x y -> y.Event.timestamp - x.Event.timestamp) notes
      @ [ (List.last notes).Event.duration ]

let group_by_beat durations ~ticks_per_beat =
  let[@tail_mod_cons] rec accumulate_up_to_beat acc current_duration durations =
    if current_duration = ticks_per_beat
    then List.rev acc :: accumulate_up_to_beat [] 0 durations
    else (
      assert (current_duration < ticks_per_beat);
      match durations with
      | [] ->
          (* End of piece, cannot complete this beat *)
          [ acc ]
      | d :: ds ->
          if current_duration + d > ticks_per_beat
          then
            let delta = ticks_per_beat - current_duration in
            if float_of_int delta  /. float_of_int ticks_per_beat <= 0.05 then
              (* Consider the beat complete if less than 5 % of a beat is
                 missing. This is a tolerance for some of MuseScore's annoying
                 shortened notes. *)
              accumulate_up_to_beat acc ticks_per_beat ds
            else
              accumulate_up_to_beat (delta :: acc) ticks_per_beat ((d - delta) :: ds)
          else accumulate_up_to_beat (d :: acc) (current_duration + d) ds)
  in
  accumulate_up_to_beat [] 0 durations

let intervals_and_durations ~ticks_per_beat voice =
  let motives =
    List.map ~f:(fun x -> x.Event.pitch) voice
    |> List.group_by ~keep_smaller_tail:false motive_size
  in
  Fmt.(debug "@[<hov 2>motives =@ %a@]\n" (list (fun f -> pf f "(%a)" (list int ~sep:sp)) ~sep:sp) motives);
  let intervals = List.map ~f:intervals motives in
  Fmt.(debug "@[<hov 2>intervals =@ %a@]\n" (list Interval_sequence.pp ~sep:sp) intervals);

  (* let actual_durations = actual_note_durations voice in
  Fmt.(debug "@[<hov 2>actual durations =@ %a@]\n" (list ~sep:sp int) actual_durations); *)
  let durations =
    List.map voice ~f:(fun ev -> ev.Event.duration) |> group_by_beat ~ticks_per_beat
  in
  (* Exclude singletons (resulting from notes lasting one beat or more) *)
  let durations = List.filter durations ~f:(fun l -> List.length l <> 1) in
  Fmt.(
    debug
      "@[<hov 2>durations grouped by beat =@ %a@]\n"
      (list ~sep:sp (fun f l -> pf f "(%a)" (list ~sep:sp int) l))
      durations);
  let duration_patterns = List.map ~f:duration_pattern durations in
  Fmt.(
    debug
      "@[<hov 2>duration_patterns =@ %a@]\n"
      (list (fun fmt i -> pf fmt "(%a)" (list float ~sep:sp) i) ~sep:sp)
      duration_patterns);

  List.combine motives intervals, List.combine durations duration_patterns

let find_signatures_generic ~cmp ~match_ motives1 motives2 =
  List.filter motives1 ~f:(fun (_, i1) ->
      List.exists motives2 ~f:(fun (_, i2) -> match_ i1 i2))
  |> List.sort_uniq ~cmp

type pair_ints_itvls = int list * Interval_sequence.t [@@deriving ord]

type pair_ints_duration_patterns = int list * float list [@@deriving ord, show]

let find_signatures =
  find_signatures_generic ~cmp:compare_pair_ints_itvls ~match_:Interval_sequence.match_

let find_duration_signatures =
  find_signatures_generic ~cmp:compare_pair_ints_duration_patterns ~match_:(fun p1 p2 ->
      List.length p1 = List.length p2
      && List.for_all2 ~f:(fun d d' -> abs_float (d -. d') <= 1.) p1 p2)

let choose_one l rand = List.nth l @@ Random.State.int rand @@ List.length l

let placate_rythms ~rand pitches ~duration_signatures =
  let rec aux to_rythm =
    let durations, _ = choose_one duration_signatures rand in
    if List.length to_rythm <= List.length durations
    then
      List.combine to_rythm (List.take (List.length to_rythm) durations)
      |> List.rev
    else
      let rythmed, to_rythm = List.split_at (List.length durations) to_rythm in
      List.rev_append (List.combine rythmed durations) (aux to_rythm)
  in
  List.rev (aux pitches)

(* Add to each note its timestamp and a constant velocity. *)
let notes_to_events ~velocity notes =
  List.fold_left_map notes ~init:0 ~f:(fun timestamp (pitch, duration) ->
      timestamp + duration, Event.{ timestamp; duration; pitch; velocity })
  |> snd

(* Round durations that are less than 2 % away from a binary multiple or
   fraction of a beat. See rants about MuseScore in other comments. *)
let fix_durations ~ticks_per_beat notes =
  List.map notes ~f:(fun note ->
    let log2_ratio =
      Float.log2 (float_of_int note.Event.duration /. float_of_int ticks_per_beat)
    in
    if abs_float ((log2_ratio -. Float.round log2_ratio) /. log2_ratio) <= 0.05 then
      { note with Event.duration = Float.(to_int (of_int ticks_per_beat *. (pow 2. (round log2_ratio)))) }
    else
      note)

let () =
  Clflags.enable_debug "main";
  Arg.parse
    [ "-o", Arg.Set_string output_file, "Set output file name" ]
    (fun filename -> input_files := !input_files @ [ filename ])
    "cope <file1> -o <output file>";
  let work1, work2 =
    match !input_files with
    | [ f1; f2 ] -> f1, f2
    | _ ->
        Fmt.epr "error: cope expects exactly 2 arguments\n";
        exit 1
  in
  if !output_file = ""
  then (
    Fmt.epr "error: option -o <output_file> is mandatory\n";
    exit 1);
  let `Raw_data data1, upper_voice1, `Ticks_per_beat tpb1 = read_first_track work1 in
  let `Raw_data _data2, upper_voice2, `Ticks_per_beat tpb2 = read_first_track work2 in
  let ticks_per_beat =
    assert (tpb1 = tpb2);
    tpb1
  in

  let upper_voice1 = fix_durations ~ticks_per_beat upper_voice1 in
  Fmt.(debug "@[<v 1>work 1 upper voice =@ %a@]\n" (list Event.pp) upper_voice1);
  let upper_voice2 = fix_durations ~ticks_per_beat upper_voice2 in
  Fmt.(debug "@[<v 1>work 2 upper voice =@ %a@]\n" (list Event.pp) upper_voice2);

  (* Just put the music in a form that's convenient for what follows. *)
  let intervals1, durations1 = intervals_and_durations ~ticks_per_beat upper_voice1 in
  let intervals2, durations2 = intervals_and_durations ~ticks_per_beat upper_voice2 in
  let signatures =
    find_signatures intervals1 intervals2
    |> List.map ~f:(fun (m, s) -> abs (Interval_sequence.direction s), (m, s))
    (* To each signature, associate an absolute "direction" *)
  in
  Fmt.(
    debug
      "@[<hov 2>signatures =@ %a@]\n"
      (list
         (fun f ->
           pf
             f
             "(%a)"
             (pair
                ~sep:(fun f () -> pf f " ->@ ")
                int
                (pair ~sep:comma (fun f -> pf f "(%a)" (list ~sep:sp int)) Interval_sequence.pp)))
         ~sep:sp)
      signatures);

  let rand = Random.State.make_self_init () in

  let backbone = Speac.generate rand ~rough_phrase_length @ [ Speac.C ] in
  let backbone =
    match backbone with
    | Speac.S :: _ -> backbone
    | _ -> Speac.S :: backbone
  in
  Fmt.(debug "@[<hov 2>backbone =@ %a@]\n" (list Speac.pp ~sep:sp) backbone);

  let melody =
    List.map backbone ~f:(fun speac_label ->
        let open Speac in
        let direction =
          match speac_label with
          | S -> 0
          | P -> 2
          | E -> 1
          | A -> 4
          | C -> 3
        in
        try List.assoc direction signatures |> fst
        with Not_found -> choose_one signatures rand |> snd |> fst)
    |> List.concat
  in
  Fmt.(debug "@[<hov 2>melody =@ %a@]\n" (list int ~sep:sp) melody);

  let duration_signatures = find_duration_signatures durations1 durations2 in
  Fmt.(
    debug
      "@[<hov 2>duration signatures =@ %a@]\n"
      (list ~sep:sp pp_pair_ints_duration_patterns)
      duration_signatures);

  let voice1 = placate_rythms ~rand melody ~duration_signatures in
  Fmt.(
    debug
      "@[<hov 2>voice1 =@ %a@]\n"
      (list ~sep:sp (fun f p -> pf f "(%a)" (pair ~sep:comma int int) p))
      voice1);
  let voice1 = notes_to_events ~velocity:64 voice1 in
  Fmt.(debug "@[<hov 2>voice1 =@ %a@]\n" (list ~sep:sp Event.pp) voice1);

  (* To write a valid MIDI file (at least, a file accepted by MuseScore), let's
     take the data of one of the input works, remove the second track, keep
     only the boring control events at the beginning of the first track, replace
     the rest, and voilà. *)
  let track_data1 = List.hd data1.Llama_midi.Data.tracks in
  let setup_event =
    List.take_while
      ~pred:(fun ev ->
        match ev with
        | Llama_midi.(
            Event.
              { message =
                  Message.Channel_voice_message
                    Channel_voice_message.{ message = Note_on _; channel = _ }
              ; delta_time = _
              }) -> false
        | _ -> true)
      track_data1
  in
  let track_to_write = setup_event @ Event.events_to_midi ~channel:0 voice1 in
  let data_to_write =
    Llama_midi.(
      Data.
        { header = { data1.header with Header.format_ = Simultaneous_tracks 1 }
        ; tracks = [ track_to_write ]
        })
  in
  debug "@[<v 2>data to write =@ %s@]\n" @@ Llama_midi.Data.to_string data_to_write;
  let writer = Llama_midi.File_writer.of_path !output_file in
  Llama_midi.File_writer.write writer data_to_write
