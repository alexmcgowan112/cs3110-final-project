type t

type coords = {
  x : int;
  y : int;
}
(** A location in 2D space *)

(** A direction in 2D space *)
type direction =
  | Up
  | Down
  | Left
  | Right

val new_room : unit -> t
(** [new_room ()] returns a new 11 by 11 room filled mostly with empty tiles and
    a few walls. *)

val move_player : t -> direction -> unit
(** [move_player room direction] attempts to move the player in [room]
    [direction]. The player will not move if they reach the edge of the room or
    if they run into a wall. *)

val to_string : t -> string
(** [to_string room] outputs a string representation of [room] *)

val get_player_pos : t -> coords
(** [get_player_pos room] returns the player's position in room. *)
