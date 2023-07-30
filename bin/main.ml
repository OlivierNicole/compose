open Cope
open StdLabels

let input_files = ref []

let output_file = ref ""

(* Size of motives considered when searching for signatures *)
let motive_size = 4

(* Length of the output, in SPEAC phrases *)
let rough_phrase_length = 10

(* We assume duple meter, i.e. division of each beat in two sub-beats. Input
   works should use this convention. Future work could include supporting
   triple meter. *)
let _meter = 2

(* Minimum length of the output work in beats. *)
let minimum_length = 20

module List : sig
  include module type of List

  val group_by : ?keep_smaller_tail:bool -> int -> 'a list -> 'a list list

  val last : 'a list -> 'a

  val take : int -> 'a list -> 'a list

  val drop : int -> 'a list -> 'a list [@@warning "-32"]

  val split_at : int -> 'a list -> 'a list * 'a list

  val take_while : pred:('a -> bool) -> 'a list -> 'a list [@@warning "-32-33"]
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
  | [] -> raise (Invalid_argument "intervals")
  | [ _ ] -> []
  | [ x; y ] -> [ f x y ]
  | x :: (y :: _ as rest) -> f x y :: generic_intervals ~f rest
  [@@tail_mod_cons]

let intervals = generic_intervals ~f:(fun x y -> y - x)

let duration_pattern = generic_intervals ~f:(fun x y -> float_of_int x /. float_of_int y)

let debug fmt =
  if Clflags.debug "main"
  then Stdlib.Format.(eprintf (fmt ^^ "%!"))
  else Stdlib.Format.(ifprintf err_formatter fmt)

let debug_duration fmt =
  if Clflags.debug "duration"
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
  Fmt.(debug "@[<v 1>read_first_track: upper voice =@ %a@]\n" (list Event.pp) voice);
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

let group_by_beat :
    type a.
       get_duration:(a -> int)
    -> with_duration:(int -> a -> a)
    -> a list
    -> ticks_per_beat:int
    -> a list list =
 fun ~get_duration ~with_duration events ~ticks_per_beat ->
  let pp_a f x = Fmt.(pf f "%d" (get_duration x)) in
  let[@tail_mod_cons] rec accumulate_up_to_beat acc current_duration events =
    if current_duration = ticks_per_beat
    then List.rev acc :: aux [] 0 events
    else (
      assert (current_duration < ticks_per_beat);
      match events with
      | [] -> (
          (* End of piece, cannot complete this beat *)
          match acc with
          | [] -> []
          | _ -> [ acc ])
      | e :: es ->
          let d = get_duration e in
          if Float.(abs (of_int d /. of_int ticks_per_beat)) <= 0.05
          then
            (* Let's not consider notes that are shorter than 5 % of a beat.
               This is a tolerance for some of MuseScore's annoying
               shortened notes. *)
            aux acc current_duration es
          else if current_duration + d > ticks_per_beat
          then
            let delta = ticks_per_beat - current_duration in
            if float_of_int delta /. float_of_int ticks_per_beat <= 0.05
            then
              (* Consider the beat complete if less than 5 % of a beat is
                 missing. This is a tolerance for some of MuseScore's annoying
                 shortened notes. *)
              aux acc ticks_per_beat events
            else
              aux
                (with_duration delta e :: acc)
                ticks_per_beat
                (with_duration (d - delta) e :: es)
          else aux (e :: acc) (current_duration + d) es)
  and aux acc current_duration events =
    Fmt.(
      debug_duration
        "accumulate (%a) %d (%a)\n"
        (list ~sep:sp pp_a)
        acc
        current_duration
        (list ~sep:sp pp_a)
        events);
    let res = accumulate_up_to_beat acc current_duration events in
    Fmt.(debug_duration "accumulate returned %a\n" (list ~sep:comma (list ~sep:sp pp_a)))
      res;
    res
  in
  accumulate_up_to_beat [] 0 events

let intervals_and_durations ~ticks_per_beat voice =
  let motives =
    List.map ~f:(fun x -> x.Event.pitch) voice
    |> List.group_by ~keep_smaller_tail:false motive_size
  in
  Fmt.(
    debug
      "@[<hov 2>motives =@ %a@]\n"
      (list (fun f -> pf f "(%a)" (list int ~sep:sp)) ~sep:sp)
      motives);
  let intervals = List.map ~f:intervals motives in
  Fmt.(debug "@[<hov 2>intervals =@ %a@]\n" (list Interval_sequence.pp ~sep:sp) intervals);

  (* let actual_durations = actual_note_durations voice in
     Fmt.(debug "@[<hov 2>actual durations =@ %a@]\n" (list ~sep:sp int) actual_durations); *)
  let durations =
    List.map voice ~f:(fun ev -> ev.Event.duration)
    |> group_by_beat ~ticks_per_beat ~get_duration:Fun.id ~with_duration:(fun d _ -> d)
  in
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

module Note = struct
  type t =
    { pitch : int
    ; duration : int
    }
  [@@deriving show]
end

let placate_rythms ~rand pitches ~duration_signatures =
  let open Note in
  let rec aux to_rythm =
    let durations, _ = choose_one duration_signatures rand in
    if List.length to_rythm <= List.length durations
    then
      List.map2
        ~f:(fun pitch duration -> { duration; pitch })
        to_rythm
        (List.take (List.length to_rythm) durations)
      |> List.rev
    else
      let rythmed, to_rythm = List.split_at (List.length durations) to_rythm in
      List.rev_append
        (List.map2 ~f:(fun pitch duration -> { pitch; duration }) rythmed durations)
        (aux to_rythm)
  in
  List.rev (aux pitches)

(* Add to each note its timestamp and a constant velocity. *)
let notes_to_events ~velocity notes =
  List.fold_left_map notes ~init:0 ~f:(fun timestamp Note.{ pitch; duration } ->
      timestamp + duration, Event.{ timestamp; duration; pitch; velocity })
  |> snd

(* Round durations that are less than 5 % away from a binary multiple or
   fraction of a beat. See rants about MuseScore in other comments. *)
let fix_durations ~ticks_per_beat notes =
  List.map notes ~f:(fun note ->
      assert (note.Event.duration > 0);
      let duration = Float.of_int note.Event.duration in
      let log2_ratio = Float.log2 (duration /. float_of_int ticks_per_beat) in
      let candidate_duration =
        Float.(of_int ticks_per_beat *. pow 2. (round log2_ratio))
      in
      if abs_float ((duration -. candidate_duration) /. duration) <= 0.1
      then
        let new_ = Float.(to_int (of_int ticks_per_beat *. pow 2. (round log2_ratio))) in
        (* debug "fixed %d into %d\n" note.Event.duration new_; *)
        { note with Event.duration = new_ }
      else (*debug "kept %d\n" note.Event.duration;*) note)

let char_array_of_string s = Array.init (String.length s) ~f:(fun i -> s.[i])

let meta_event type_index contents =
  let open Llama_midi in
  Message.Meta_event
    Meta_event.(Other { type_index; contents = char_array_of_string contents })

let chan_msg channel message =
  let open Llama_midi in
  Message.Channel_voice_message Channel_voice_message.{ channel; message }

let transpose ~shift notes =
  List.map notes ~f:(fun note ->
      let pitch = if note.Note.pitch = 0 then 0 else note.Note.pitch + shift in
      { note with Note.pitch })

(* Assumes 1 bar = 2 beats *)
let[@tail_mod_cons] rec map_every_other_bar_even ~f = function
  | b1 :: b2 :: bs -> f b1 :: f b2 :: map_every_other_bar_odd ~f bs
  | [ beat ] -> [ f beat ]
  | [] -> []

and[@tail_mod_cons] map_every_other_bar_odd ~f = function
  | b1 :: b2 :: bs -> b1 :: b2 :: map_every_other_bar_even ~f bs
  | [ _ ] as beat -> beat
  | [] -> []

(* C major scale starting from C0 *)
let major_scale =
  let next_in_c_major n =
    let degree = n mod 12 in
    let octave = n / 12 in
    let next_degree =
      match degree with
      | 0 -> 2 (* C -> D *)
      | 2 -> 4 (* D -> E *)
      | 4 -> 5 (* E -> F *)
      | 5 -> 7 (* F -> G *)
      | 7 -> 9 (* G -> A *)
      | 9 -> 11 (* A -> B *)
      | 11 -> 12 (* B -> C *)
      | _ -> assert false (* Not in the scale *)
    in
    (octave * 12) + next_degree
  in
  Seq.iterate next_in_c_major 0

(* This is an auxiliary function of the next function but is also used as a
   standalone function for choosing notes from a chord, see [insert_cadence].
   This is a copy of Cope's. *)
let rec find_closest note other_note right_notes =
  match Seq.uncons right_notes with
  | None -> assert false
  | Some (first, right_notes) -> (
      match Seq.uncons right_notes with
      | None -> assert false
      | Some (second, right_notes) ->
          if second >= note
          then
            if other_note - first <> 5 && note - first <= second - note
            then first
            else second
          else find_closest note other_note right_notes)

(* This weird and complicated function is almost a direct copy of Cope's
   function of the same name. If I'm not mistaken, it returns a pitch either a
   third above or a third below the given note. The third may be major or
   minor, but it is such that the resulting note is in the scale of C major. *)
let find_closest_consonant ~rand base_note =
  find_closest
    (choose_one (List.filter ~f:(fun n -> n >= 0) [ base_note - 4; base_note + 4 ]) rand)
    base_note
    (Seq.take 100 major_scale)

(* Notes of the C major chord on all octaves *)
let tonic_chord =
  let next n =
    let degree = n mod 12 in
    let octave = n / 12 in
    let next_degree =
      match degree with
      | 0 -> 4 (* C -> E *)
      | 4 -> 7 (* E -> G *)
      | 7 -> 12 (* G -> C of next octave *)
      | _ -> assert false
    in
    (octave * 12) + next_degree
  in
  Seq.iterate next 0 |> Seq.take 40

(* Notes of the G major chord on all octaves *)
let dominant_chord = Seq.map (( + ) 7) tonic_chord

let tonic_notes = Seq.iterate (( + ) 12) 0 |> Seq.take 10

let member_dominant_chord note = Seq.exists (fun n -> note = n) dominant_chord

let quarter ~ticks_per_beat pitch = Note.{ pitch; duration = ticks_per_beat }

let rec insert_cadence
    ~ticks_per_beat
    ~minimum_length
    (beats : (Note.t list * Note.t list) list) =
  let open Note in
  match beats with
  | [] ->
      Printf.eprintf
        "Could not insert cadence: the work was shorter than %d beats.\n"
        minimum_length;
      []
  | beat :: beats when minimum_length > 0 ->
      beat :: insert_cadence ~ticks_per_beat ~minimum_length:(minimum_length - 1) beats
  | (upper_beat, lower_beat) :: beats -> (
      match upper_beat, lower_beat with
      | { pitch = high_note; duration = _ } :: _, { pitch = low_note; duration = _ } :: _
        ->
          if member_dominant_chord low_note && member_dominant_chord high_note
          then
            [ ( upper_beat
                @ [ quarter ~ticks_per_beat
                    @@ find_closest high_note high_note tonic_chord
                  ]
              , lower_beat
                @ [ quarter ~ticks_per_beat @@ find_closest low_note low_note tonic_notes
                  ] )
            ]
          else
            (upper_beat, lower_beat)
            :: insert_cadence ~ticks_per_beat ~minimum_length:(minimum_length - 1) beats
      | _ ->
          raise
            (Failure "insert_cadence: encountered an empty beat, which shouldn't happen"))

let seed = ref 0

let debug_option = ref ""

let enable_debug option =
  let options = String.split_on_char ~sep:',' option in
  List.iter options ~f:(fun s -> Clflags.enable_debug s)

let () =
  Arg.parse
    [ "-o", Arg.Set_string output_file, "Output file name"
    ; "-seed", Arg.Set_int seed, "Random seed"
    ; ( "-debug"
      , Arg.Set_string debug_option
      , "Comma-separated list of passes to enable debug output for" )
    ]
    (fun filename -> input_files := !input_files @ [ filename ])
    "cope <input work 1> <input work 2> -o <output file>";
  let work1, work2 =
    match !input_files with
    | [ f1; f2 ] -> f1, f2
    | _ ->
        Fmt.epr "error: cope expects exactly 2 arguments\n";
        exit 1
  in
  enable_debug !debug_option;
  if !output_file = ""
  then (
    Fmt.epr "error: option -o <output_file> is mandatory\n";
    exit 1);
  let `Raw_data _data1, upper_voice1, `Ticks_per_beat tpb1 = read_first_track work1 in
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
                (pair
                   ~sep:comma
                   (fun f -> pf f "(%a)" (list ~sep:sp int))
                   Interval_sequence.pp)))
         ~sep:sp)
      signatures);

  if !seed = 0
  then (
    Random.self_init ();
    seed := Random.bits ());
  let rand = Random.State.make [| !seed |] in

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
      (list ~sep:sp (fun f p -> pf f "(%a)" Note.pp p))
      voice1);

  (***** Creating the second voice *****)

  (* Group the upper voice by beat *)
  let rest1 = Note.{ pitch = 0; duration = ticks_per_beat } in
  let voice1 =
    group_by_beat
      voice1
      ~get_duration:(fun e -> e.Note.duration)
      ~with_duration:(fun duration e -> Note.{ e with duration })
      ~ticks_per_beat
  in
  Fmt.(
    debug
      "@[<v 2>voice1 =@ %a@]\n"
      (list ~sep:(fun f () -> pf f "//@,") (list ~sep:sp Note.pp))
      voice1);

  (* Create the second voice by duplicating the first and offsetting it with a
     bar of rest at the beginning *)
  let voice2 = [ rest1 ] :: [ rest1 ] :: voice1 in
  Fmt.(
    debug
      "@[<v 2>voice2 =@ %a@]\n"
      (list ~sep:(fun f () -> pf f "//@,") (list ~sep:sp Note.pp))
      voice2);

  let voice1 = voice1 @ [ [ rest1 ]; [ rest1 ] ] in
  let beats = List.combine voice1 voice2 in
  (* Replace every other bar in upper voice with countersubject, starting with
     the second bar *)
  let beats =
    map_every_other_bar_odd beats ~f:(fun (_, lower_beat) ->
        ( [ (match (List.hd lower_beat).Note.pitch with
            | 0 -> quarter ~ticks_per_beat 0
            | pitch -> quarter ~ticks_per_beat @@ find_closest_consonant ~rand pitch)
          ]
        , lower_beat ))
  in
  (* Replace every other bar in lower voice with countersubject, starting with the third bar *)
  let beats =
    match beats with
    | b1 :: b2 :: rest ->
        b1
        :: b2
        :: map_every_other_bar_odd rest ~f:(fun (upper_beat, _) ->
               ( upper_beat
               , [ (match (List.hd upper_beat).Note.pitch with
                   | 0 -> quarter ~ticks_per_beat 0
                   | pitch ->
                       quarter ~ticks_per_beat @@ find_closest_consonant ~rand pitch)
                 ] ))
    | _ ->
        (* The melody is too short. Something went wrong in the previous steps. *)
        assert false
  in
  (* Insert a cadence *)
  let beats = insert_cadence ~ticks_per_beat ~minimum_length beats in
  let voice1, voice2 = List.split beats in
  let voice1 = List.concat voice1 in
  let voice2 = List.concat voice2 |> transpose ~shift:(-12) in

  (***** Writing the output to disk *****)
  let voice1 = notes_to_events ~velocity:64 voice1 in
  (* Fmt.(debug "@[<hov 2>voice1 =@ %a@]\n" (list ~sep:sp Event.pp) voice1); *)
  let voice2 = notes_to_events ~velocity:64 voice2 in

  let ( ^:: ) message evs = Llama_midi.(Event.{ delta_time = 0; message }) :: evs in

  let voice1 =
    let open Llama_midi in
    meta_event 4 "Clavecin"
    (* Track name *)
    ^:: meta_event 88 "\004\002\024\016"
    (* Time signature: 4/4, 24 clocks per quarter note, 32 32nds per quarter note *)
    ^:: meta_event 89 "\000\000"
    (* Key signature: C major *)
    ^:: meta_event 81 "\010\165\074"
    (* Tempo: 86 bpm *)
    ^:: chan_msg 0 Channel_voice_message.(Control_change { controller = 121; value = 0 })
    (* All controllers off *)
    ^:: chan_msg 0 Channel_voice_message.(Program_change { program = 6 })
    (* Set instrument to harpsichord *)
    ^:: chan_msg 0 Channel_voice_message.(Control_change { controller = 7; value = 100 })
    (* Set volume *)
    ^:: chan_msg 0 Channel_voice_message.(Control_change { controller = 10; value = 64 })
    (* Set Pan (left-right balance) to centered *)
    ^:: chan_msg 0 Channel_voice_message.(Control_change { controller = 91; value = 0 })
    (* Reverb off *)
    ^:: chan_msg 0 Channel_voice_message.(Control_change { controller = 93; value = 0 })
    (* Chorus off *)
    ^:: meta_event 33 "\000"
    (* play on MIDI port 0 *)
    ^:: Cope.Event.events_to_midi ~channel:0 voice1
  in
  let voice2 =
    let open Llama_midi in
    meta_event 4 "Clavecin"
    (* Track name *)
    ^:: meta_event 89 "\000\000"
    (* Key signature: C major *)
    ^:: chan_msg 1 Channel_voice_message.(Control_change { controller = 121; value = 0 })
    (* All controllers off *)
    ^:: chan_msg 1 Channel_voice_message.(Program_change { program = 6 })
    (* Set instrument to harpsichord *)
    ^:: chan_msg 1 Channel_voice_message.(Control_change { controller = 7; value = 100 })
    (* Set volume *)
    ^:: chan_msg 1 Channel_voice_message.(Control_change { controller = 10; value = 64 })
    (* Set Pan (left-right balance) to centered *)
    ^:: chan_msg 1 Channel_voice_message.(Control_change { controller = 91; value = 0 })
    (* Reverb off *)
    ^:: chan_msg 1 Channel_voice_message.(Control_change { controller = 93; value = 0 })
    (* Chorus off *)
    ^:: meta_event 33 "\000"
    (* play on MIDI port 0 *)
    ^:: Cope.Event.events_to_midi ~channel:1 voice2
  in
  let data_to_write =
    Llama_midi.(
      Data.
        { header =
            Header.
              { format_ = Simultaneous_tracks 2
              ; division = Division.Ticks_per_quarter_note ticks_per_beat
              }
        ; tracks = [ voice1; voice2 ]
        })
  in
  debug "@[<v 2>data to write =@ %s@]\n" @@ Llama_midi.Data.to_string data_to_write;
  let writer = Llama_midi.File_writer.of_path !output_file in
  Llama_midi.File_writer.write writer data_to_write
