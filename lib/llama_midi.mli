module Unimplemented : sig
  type t = Unimplemented of string
end

module Channel_voice_message : sig
  type note_event = { note : int; velocity : int }
  type control_change = { controller : int; value : int }
  type all_notes_off_event =
    | Omni_mode_on
    | Omni_mode_off
    | Mono_mode_on of int
    | Poly_mode_on
  type message =
    | Note_off of note_event
    | Note_on of note_event
    | Program_change of int
    | Control_change of control_change
    | Local_control_off
    | Local_control_on
    | All_notes_off of all_notes_off_event option
  type t = { channel : int; message : message }
end

module Meta_event : sig
  type other = { type_index : int; contents : char array }
  type t = End_of_track | Other of other
end

module Message : sig
  type t =
    | Channel_voice_message of Channel_voice_message.t
    | Meta_event of Meta_event.t
end

module Event : sig
  type t = { delta_time : int; message : (Message.t, Unimplemented.t) result }

  val to_string : t -> string
end

module Track : sig
  type t = Event.t list
end

module Format : sig
  type t =
    | Single_track
    | Simultaneous_tracks of int
    | Sequential_tracks of int
end

module Division : sig
  type time_code = { smpte_format : int; ticks_per_frame : int }
  type t = Ticks_per_quarter_note of int | Time_code of time_code
end

module Header : sig
  type t = { format_ : Format.t; division : Division.t }
end

module Data : sig
  type t = { header : Header.t; tracks : Track.t list }

  val to_string : t -> string
end

module File_reader : sig
  type t

  val of_path : string -> t
  val read : t -> Data.t
end

module File_writer : sig
  type t

  val of_path : string -> t
  val write : t -> Data.t -> unit
end
