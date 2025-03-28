type t

val new_room : unit -> t
(** [new_room ()] returns a new 11 by 11 room filled mostly with empty tiles and
    a few walls. *)

val load_room_from_file : string -> t
(** [load_room_from_file filename] creates a room from the json file at
    [filename]. *)

(* TODO: It would probably make more sense to turn this into a take_action
   function, where there are actions avaliable besides just moving. This would
   require changing direction too.*)
val move_player : t -> Keyboard.t -> unit
(** [move_player room direction] attempts to move the player in [room]
    [direction]. The player will not move if they reach the edge of the room or
    if they run into a wall. *)

val to_string : t -> string
(** [to_string room] outputs a string representation of [room] *)

val to_string_matrix : t -> string array array
(** [to_string_array room] outputs a string array array representation of [room]
*)

val get_player_pos : t -> Coords.t
(** [get_player_pos room] returns the player's position in room. *)

val explode : t -> unit
(** [explode room] updates a currently happening explosion in the current room.
*)

val start_exploding : t -> Coords.t -> int -> unit
(** [start_exploding room position radius] starts an explosion centered at
    [position] that will grow to a maximum radius of [radius]. *)

val exploding : t -> bool
(** [exploding room] is whether or not an explosion is currently happening in
    the provided room. *)

val place_bomb : t -> unit
(** [place_bomb room] creates a bomb at the player's current location. *)
