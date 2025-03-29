type t

val create : Coords.t -> int -> t
val spread : t -> unit
val tile_is_exploding : Coords.t -> t -> bool
val is_in_progress : t -> bool
