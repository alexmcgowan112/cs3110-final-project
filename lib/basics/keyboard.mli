(** The type [t] represents keyboard inputs. *)
type t =
  | Up
  | Down
  | Left
  | Right
  | B
  | Q
  | None
  | Space

val to_string : t -> string
(** [to_string input] converts [input] to its string representation. *)

(** Functor [MakeInput] creates a module for handling keyboard input. *)
module MakeInput : functor
  (G : sig
     val get : unit -> Curses.attr_t
     (** [get ()] retrieves the current attribute of the terminal. *)
   end)
  -> sig
  val read_input : unit -> t
  (** [read_input ()] reads a keyboard input and returns its corresponding type
      [t]. *)
end
