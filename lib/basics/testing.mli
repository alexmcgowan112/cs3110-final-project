module TestInput : sig
  val read_input : unit -> Keyboard.t
  (** [read_input ()] reads a keyboard input and returns it. *)
end

val string_of_list : ('a -> string) -> 'a list -> string
(** [string_of_list f l] converts [l] to a string by applying the function [f]
    to each element and joining the results with commas. *)

val input_responses : unit -> Keyboard.t list
(** [input_responses ()] returns a sequence of keyboard inputs up to
    Keyboard.None (indicating no further input). *)

val run_inputs : Keyboard.t list -> Room.t
(** [run_inputs inputs] simulates the sequence of actions in [inputs] in the
    game and then returns the final state of the room. *)
