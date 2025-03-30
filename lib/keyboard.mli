(** The type [t] represents keyboard inputs. *)
type t =
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | B
  | Q
  | None

(** Functor [MakeInput] creates a module for handling keyboard input. *)
module MakeInput : functor
  (G : sig
     val get : unit -> Curses.attr_t
     (** [get] retrieves the current attribute of the terminal. *)
   end)
  -> sig
  val read_input : unit -> t
  (** [read_input] reads a keyboard input and returns its corresponding type
      [t]. *)

  val string_of_input : t -> string
  (** [string_of_input] converts a keyboard input of type [t] to its string
      representation. *)
end
