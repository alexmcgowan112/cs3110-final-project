val move_player : Dungeon.t -> Keyboard.t -> unit
(** [move_player room key] moves the player within [room] based on [key]. *)

val test_input_handling : Dungeon.t -> Keyboard.t -> unit
(** [test_input_handling room input] processes [input] for the given [room],
    handling interactions such as movement or other actions based on the current
    state of the room. This function is a wrapper for use in testing.*)

val process_world: Dungeon.t -> unit
(** [proccess_world room] processes the game world, updating the state of the
    game based on the current room and player actions. *)
