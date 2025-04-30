type stats =
  | Item
  | Armor of { def : int }
  | Weapon of { atk : int }

type t = {
  id : int;
  stats : stats;
  mutable location : (Coords.t * Room.t) option;
}
(** AF: [{id; stats; location}] represents the specific item represented by
    [id], which has [stats] and is at [location]. RI: [id] is a valid item
    id(what this means is tbd) *)

let get_defense armor =
  match armor.stats with
  | Armor s -> Some s.def
  | _ -> None

let get_attack weapon =
  match weapon.stats with
  | Weapon s -> Some s.atk
  | _ -> None

let get_item_id item = item.id

let get_item_location dungeon item =
  match item.location with
  | Some loc -> loc
  | None ->
      let r = Dungeon.current_room dungeon in
      (Room.get_player_pos r, r)

let drop_item item dungeon =
  let room = Dungeon.current_room dungeon in
  let player_pos = Room.get_player_pos room in
  item.location <- Some (player_pos, room)

let pickup_item item = item.location <- None
let create_item id stats location = { id; stats; location }
