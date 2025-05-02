type stats =
  | Item
  | Armor of { def : int ref }
    (* int ref so we can deplete how much "health" this piece of armor has as it
       gets hit*)
  | BiggerRadius
  | ShorterFuse
  | Health
  | Bomb

type t = {
  id : int;
  stats : stats;
  mutable location : Coords.t option;
}
(** AF: [{id; stats; location}] represents the specific item represented by
    [id], which has [stats] and is at [location]. RI: [id] is a valid item
    id(what this means is tbd) *)

let get_defense armor =
  match armor.stats with
  | Armor s -> Some !(s.def)
  | _ -> None

let get_item_id item = item.id

(** [Some coords] if in a room, [None] if on the player. Currently, the item
    doesn't know what room it's in*)
let get_location item = item.location

let pickup_item item = item.location <- None
let create_item id stats location = { id; stats; location }
let stats item = item.stats

let to_string item =
  match item.stats with
  | Item -> "+"
  | Armor def -> "v"
  | BiggerRadius -> "$"
  | ShorterFuse -> "~"
  | Health -> "H"
  | Bomb -> "B"
