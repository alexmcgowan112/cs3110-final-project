type t

val create : Coords.t -> t
(** [create pos] makes a new enemy at [pos] *)

val move : t -> Coords.t -> unit
(** [move enemy pos] moves [enemy] to [pos] *)

val get_position : t -> Coords.t
(** [get_position enemy] gets the current position of [enemy]. *)

val next_move : Coords.t -> t -> t list -> Coords.t
(** [next_move player_loc enemy all_enemies] calculates a move for [enemy] based
    on [target] and [all_enemies]. This isn't used yet but can theoretically be
    called by room. *)

val take_damage : t -> int -> unit
(** [take_damage enemy amount] subtracts [amount] from the health of [enemy]. *)

val is_alive : t -> bool
(** [is_alive enemy] determines if the health of [enemy] is still above 0. *)

val move_or_attack : t -> Coords.t -> Player.t -> t list -> t
(** [move_or_attack enemy player_loc player all_enemies] updates [enemy], based
    off what it did this turn. [player] is modified in place if it took damage
    from [enemy]. *)
