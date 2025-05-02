open Connections

type t

val create : Coords.t -> int -> t
(** [create coords max_radius] creates a new explosion at [coords] with a
    maximum potential radius of [max_radius]. [max_radius] must be > 0*)

val is_in_progress : t -> bool
(** [is_in_progress explosion] returns true if [explosion] is still in progress.
*)

val spread : t -> unit
(** [spread explosion] updates [explosion] to increase in radius. If [explosion]
    has reached its maximum radius, it will not spread any further, and will no
    longer be "in progress" after the next time it is spread. *)

val tile_is_exploding : Coords.t -> t list -> G.t -> bool
(** [tile_is_exploding coords explosions] returns true if the tile at [coords]
    is currently exploding in any explosion in [explosions]. UPDATE:
    [tile_is_exploding] now takes the list of all explosions in a given room for
    simplicity. UPDATE: [tile_is_exploding] takes a graph of the room so that it
    can bfs around walls. *)

val location : t -> Coords.t
(** [location explosion] returns the coordinates of the explosion. *)
