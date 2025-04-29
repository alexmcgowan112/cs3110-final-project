type t

val health : t -> int
(** [health player] is the provided player's health *)

val create : unit -> t
(** [create ()] is a default player (has 5 health)*)

val damage : t -> int -> unit
(** [damage player dmg_amount] modifes the provided player to reduce its health by [dmg_amount]*)

val is_alive : t -> bool
(** [is_alive player] is whether or not the given player is alive (i.e. health > 0)*)