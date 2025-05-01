type t

val new_room : unit -> t
(** [new_room ()] returns a new 11 by 11 room filled mostly with empty tiles and
    a few walls. *)

val load_room_from_file : string -> t
(** [load_room_from_file filename] creates a room from the json file at
    [filename]. *)

val wait : t -> Player.t -> unit
(** [wait room] advances time without moving the player. *)

val move_player : t -> Keyboard.t -> Player.t -> unit
(** [move_player room direction] attempts to move the player in [room]
    [direction]. The player will not move if they reach the edge of the room or
    if they run into a wall. *)

val update_enemies : t -> Player.t -> unit
(** [update_enemies room player] updates the enemies in the provided room
    in-place*)

val get_enemies : t -> Enemies.t option array
(** [get_enemies room] is the list of Enemies in the provided room. *)

val explode : t -> unit
(** [explode room] updates all currently happening explosions in [room]. *)

val exploding : t -> bool
(** [exploding room] is whether or not an explosion is currently happening in
    [room] *)

val place_bomb : t -> Player.t -> unit
(** [place_bomb room] creates a bomb at the player's current location. *)

val get_player_pos : t -> Coords.t
(** [get_player_pos room] returns the player's position in [room]. *)

val set_player_pos : t -> Coords.t -> unit
(** [set_player_pos room location] puts the player in the given room at the
    given location. This is used by the dungeon to move players between rooms.
*)

val to_string_matrix : t -> string array array
(** [to_string_matrix room] outputs a string representation of [room] as a
    matrix. Each tile is represented by a single character. *)

val update_items : t -> Player.t -> unit
(**[update_items room player] equips the item the player is standing on to the
   player and removes it from the room (assuming the player is standing on an
   item)*)
