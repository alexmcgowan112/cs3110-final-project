val move_player : Dungeon.t -> Keyboard.t -> unit
(** [move_player room key] moves the player within [room] based on [key]. *)

val test_input_handling : Dungeon.t -> Keyboard.t -> unit
(** [test_input_handling room input] processes [input] for the given [room],
    handling interactions such as movement or other actions based on the current
    state of the room. This function is a wrapper for use in testing.*)

val handle_input : Dungeon.t -> unit
(** [handle_input room] manages the input handling for the given [room],
    handling movement or other actions in the current room.*)
