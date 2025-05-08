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

val generate : int * int * string -> int -> t * Coords.t list
(** [generate (width, height, room_file) num_exits ] actually generate a room
    given the width of the room, height of the room, the \*.rm file the room
    will use, and the number of exits to be placed in the room. Returns a room
    and a list of coords corresponding to the location of each exit*)

val pick_room_file : unit -> int * int * string
(** [pick_room_tile ()] returns (width, height, room_file_name) by choosing a
    random \*.rm file*)

val get_empty_tiles : t -> Coords.t list
(** [get_empty_tiles room] is the list of all the Coords of empty tiles in the
    provided room*)

val generate : int * int * string -> int -> t * Coords.t list
(** [generate (width, height, room_file) num_exits ] actually generate a room
    given the width of the room, height of the room, the \*.rm file the room
    will use, and the number of exits to be placed in the room. Returns a room
    and a list of coords corresponding to the location of each exit*)

val pick_room_file : unit -> int * int * string
(** [pick_room_tile ()] returns (width, height, room_file_name) by choosing a
    random \*.rm file*)

val get_empty_tiles : t -> Coords.t list
(** [get_empty_tiles room] is the list of all the Coords of empty tiles in the
    provided room*)

val update_player_health : t -> Player.t -> unit
(** [update_player_health room player] updates the player's health based on
    the items they are standing on. If the player is standing on a health item,
    it will be added to their health. If the player is standing on a damage item,
    it will be subtracted from their health. *)