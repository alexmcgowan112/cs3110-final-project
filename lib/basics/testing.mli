module TestInput : sig
  val read_input : unit -> Keyboard.t
  (** [read_input] reads a keyboard input and returns its corresponding type
      [t]. *)

  val string_of_input : Keyboard.t -> string
  (** [string_of_input] converts a keyboard input of type [t] to its string
      representation. *)
end

val string_of_list : ('a -> string) -> 'a list -> string
(** [string_of_list f l] converts a list [l] of elements of type ['a] to a
    string by applying the function [f] to each element and joining the results
    with commas. *)

val input_responses : unit -> Keyboard.t list
(** [input_responses ()] reads a sequence of keyboard inputs until a the inputs
    return Keyboard.None (indicating no further input). It uses a reference to
    track the current input and collects the inputs in a list.

    @return
      A list of keyboard inputs read from the array in the above input module.
*)

val run_inputs : Keyboard.t list -> Room.t
(** [run_inputs inputs] takes a list of keyboard inputs and simulates the
    corresponding actions in the game. It uses the input module to handle the
    inputs and returns the final state of the room after processing all inputs.

    @param inputs A list of keyboard inputs to be processed.
    @return The final state of the room after processing all inputs. *)
