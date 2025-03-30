val move_player : Room.t -> Keyboard.t -> unit
(** [move_player room key] moves the player within the given [room] based on the
    input [key] from the keyboard. The movement is determined by the current
    state of the room and the key pressed. *)

val test_input_handling : Room.t -> Keyboard.t -> unit
(** [test_input_handling room input] processes user [input] for the given
    [room], handling interactions such as movement or other actions based on the
    current state of the room. This function is a wrapper for use in testing.*)

val handle_input : Room.t -> unit
(** [handle_input room] manages the input handling for the given [room],
    handling movement or other actions in the current room.*)
