val move_player : Room.t -> Keyboard.t -> unit
(** [move_player room key] moves the player within the given [room] based on the
    input [key] from the keyboard. The movement is determined by the current
    state of the room and the key pressed. *)

val input_handling : Room.t -> unit
(** [input_handling room] processes user input for the given [room], handling
    interactions such as movement or other actions based on the current state of
    the room. *)
