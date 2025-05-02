(* my plan for making Player.ml: the Player is basically just a list of stats.
   Its location is managed by the Dungeon/Room. Subject to change. *)
type t = {
  mutable health : int;
  mutable items : Item.t list;
  mutable bombs : int;
  base_fuse_time : int;
  base_blast_radius : int;
}

let health player = player.health

let create () =
  { health = 5; items = []; base_fuse_time = 6; base_blast_radius = 1; bombs = 5 }

let add_bombs player num_bombs =
  player.bombs <- player.bombs + num_bombs

let remove_bombs player num_bombs =
  player.bombs <- max 0 (player.bombs - num_bombs)

let bombs player = player.bombs

let total_armor player =
  List.fold_left
    (fun acc item ->
      match Item.stats item with
      | Armor { def } -> acc + !def
      | _ -> acc)
    0 player.items

let fuse_decrease_from_items player =
  List.fold_left
    (fun acc item ->
      match Item.stats item with
      | ShorterFuse -> acc + 1
      | _ -> acc)
    0 player.items

let radius_increase_from_items player =
  List.fold_left
    (fun acc item ->
      match Item.stats item with
      | BiggerRadius -> acc + 1
      | _ -> acc)
    0 player.items

let min_fuse_time = 1

let fuse_time player =
  max min_fuse_time player.base_fuse_time - fuse_decrease_from_items player

let max_possible_blast_radius = 100

let blast_radius player =
  min max_possible_blast_radius player.base_blast_radius
  + radius_increase_from_items player

(**[dmg_to_armor player dmg_amount] spreads the damage over the player's armor
   as much as possible and returns the amount of damage left to be applied to
   the player's health*)
let dmg_to_armor player dmg_amount =
  max 0
    (List.fold_left
       (fun remaining_dmg item ->
         match Item.stats item with
         | Armor { def } ->
             let absorb = min remaining_dmg !def in
             def := !def - absorb;
             remaining_dmg - absorb
         | _ -> remaining_dmg)
       dmg_amount player.items)

(**[remove_depleted_armor player] removes all the equipped armor that has 0
   defense (ie is completly used up)*)
let remove_depleted_armor player =
  player.items <-
    List.filter
      (fun item ->
        match Item.stats item with
        | Armor { def } -> !def > 0
        | _ -> true)
      player.items

let damage player dmg_amount =
  let dmg_after_armor = dmg_to_armor player dmg_amount in
  remove_depleted_armor player;
  player.health <- max 0 (player.health - dmg_after_armor)

let is_alive player = player.health > 0
let equip player item = player.items <- item :: player.items
