type t
(* the type of a dungeon*)

type room_exit
(* the type of an exit leading from one room to another within the dungeon*)

val create : unit -> t
(** [create ()] is a new dungeon. It currently has 2 rooms and one exit
    connecting each room. *)

val load_dungeon_from_file : string -> t
(** [load_dungeon_from_file path] creates a dungeon from the file at [path]. *)

val create_test : unit -> t
(** [create_test ()] is a new dungeon to use for testing. *)

val current_room : t -> Room.t
(** [current_room dungeon] is the current [Room] that the player is inside of.
*)

val move_player : t -> Keyboard.t -> unit
(** [move_player dungeon direction] moves the player in the given direction
    inside the given dungeon. *)

val hud_text : t -> string
(** [hud_text dungeon] returns the text for the hud bar at the bottom of the
    screen.*)

val set_hud_text : t -> string -> unit
(** [set_hud_text dungeon text] sets the text for the hud bar at the bottom of
    the screen. *)

val player : t -> Player.t
(* [player dungeon] is the [Player] that is in the provided dungeon. *)

val generate : unit -> t
(* [generate ()] is a randomly generated dungeon *)