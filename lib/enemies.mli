type t

val create : Coords.t -> int -> t
(**xposition->yposition->id->enemy*)
val move : t -> Coords.t -> unit
(**move enemy to xposition yposition*)
val get_position : t -> Coords.t
(**get enemy position*)
val next_move : Coords.t -> t -> t list -> Coords.t
(**gets the next move of the enemy, this will be called in room (maybe?)*)
val take_damage : t -> int -> unit
(**takes damage from player/bomb etc.*)
val is_alive : t -> bool
(**is alive, removed from enemies list if false*)

val move_or_attack : t -> Coords.t -> Player.t -> t list -> t
(** [move_or_attack enemy player_loc player all_enemies] is the provided enemy, 
updated based off what it did this turn. The Player is modified in place 
if it took damage from this enemy. *)
