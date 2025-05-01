val move_player : Dungeon.t -> Keyboard.t -> unit
(** [move_player room key] moves the player within [room] based on [key]. *)

val test_input_handling :
  ?cmd_palette_str:string -> Dungeon.t -> Keyboard.t -> string option
(** [test_input_handling ?cmd_palette_str room input] processes [input] for the
    given [room], handling interactions such as movement or other actions based
    on the current state of the room. If [?cmd_palette_str] is provided, it is
    the string that will be passed to the command palette if it is triggered.
    This function is a wrapper for use in testing. It returns an option
    containing the string to be displayed on the command palette, if any.*)

val process_world : Dungeon.t -> bool
(** [proccess_world room] processes the game world, updating the state of the
    game based on the current room and player actions. If [false], the game is
    over. Otherwise, it continues. *)
