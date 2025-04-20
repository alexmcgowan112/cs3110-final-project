type t
(** The type of a dungeon. *)

val create : unit -> t
(** [create ()] is a new dungeon. It currently has 2 rooms and one exit
    connecting each room. *)

val load_dungeon_from_file : string -> t

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
