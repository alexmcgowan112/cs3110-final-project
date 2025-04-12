type t

val create : int -> int -> t
(**xposition->yposition->enemy*)
val move : t -> int -> int -> unit
(**move enemy to xposition yposition*)
val get_position : t -> int * int
(**get enemy position*)
val next_move : Room.t -> t -> int * int
val take_damage : t -> int -> unit
val is_alive : t -> bool