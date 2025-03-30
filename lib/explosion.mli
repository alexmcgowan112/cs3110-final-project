type t

val create : Coords.t -> int -> t
(** [create coords max_radius] creates a new explosion at the given coordinates
    with a maximum potential radius of [max_radius]. *)

val spread : t -> unit
(** [spread explosion] updates the explosion to spread to the next tile. If the
    explosion has reached its maximum radius, it will not spread any further. *)

val tile_is_exploding : Coords.t -> t -> bool
(** [tile_is_exploding coords explosion] returns true if the tile at [coords] is
    currently exploding in any explosion. *)

val is_in_progress : t -> bool
(** [is_in_progress explosion] returns true if the explosion is still in
    progress. *)
