type stats =
  | Item
  | Armor of { def : int ref }
  | Weapon of { atk : int }

type t

val get_defense : t -> int option
(** [get_defense item] returns the defense value of the given [item] if it is an
    armor, or [None] if it is not. *)

val get_attack : t -> int option
(** [get_attack item] returns the attack value of the given [item] if it is a
    weapon, or [None] if it is not. *)

val get_item_id : t -> int
(** [get_item_id item] returns the unique identifier of the given [item]. *)

val get_location : t -> Coords.t option
(** [get_location dungeon item] returns the current location of the given [item]
    in the form of a tuple [(coords, room)], where [coords] is the position of
    the item in the room, and [room] is the room it is located in. If the item
    is not placed in any room, it defaults to the player's position in the
    current room of the given [dungeon]. *)

(* val drop_item : t -> Dungeon.t -> unit *)
(** [drop_item item dungeon] places the given [item] at the player's current
    position in the current room of the given [dungeon]. *)

val pickup_item : t -> unit
(** [pickup_item item] removes the given [item] from its current location,
    making it no longer associated with any room. *)

val create_item : int -> stats -> Coords.t option -> t
(** [create_item id stats location] creates a new item with the given [id],
    [stats], and initial [location]. *)

val to_string : t -> string
(**[to_string item] is the string representation*)


val clear_location : t -> unit
(**[clear_location item] clears an item's coordinates (for when a player equips it)*)

val stats : t -> stats
(**[stats item] is the items Stats*)
