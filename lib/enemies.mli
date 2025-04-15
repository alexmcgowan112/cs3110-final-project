type t

val create : Coords.t -> int -> t
(**xposition->yposition->id->enemy*)
val move : t -> Coords.t -> unit
(**move enemy to xposition yposition*)
val get_position : t -> Coords.t
(**get enemy position*)
val next_move : Room.t -> t -> Coords.t
(**gets the next move of the enemy, this will be called in room (maybe?)*)
val take_damage : t -> int -> unit
(**takes damage from player/bomb etc.*)
val is_alive : t -> bool
(**is alive, removed from enemies list if false*)