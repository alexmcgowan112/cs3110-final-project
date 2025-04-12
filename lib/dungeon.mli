type t
(** The type of a dungeon. *)

val create : unit -> t
(** [create ()] is a new dungeon. It currently has 2 rooms and one exit
    connecting each room. *)

val create_test : unit -> t
(** [create_test ()] is a new dungeon to use for testing. *)

val current_room : t -> Room.t
(** [current_room dungeon] is the current [Room] that the player is inside of.
*)

val move_player : t -> Keyboard.t -> unit
(** [move_player dungeon direction] moves the player in the given direction
    inside the given dungeon. *)
