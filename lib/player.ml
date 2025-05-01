(* my plan for making Player.ml: the Player is basically just a list of stats.
   Its location is managed by the Dungeon/Room. Subject to change. *)
type t = {
  mutable health : int;
  mutable items : Item.t list;
}

let health player = player.health
let create () = { health = 5; items = [] }

let total_armor player =
  List.fold_left
    (fun acc item ->
      match Item.stats item with
      | Armor { def } -> acc + !def
      | _ -> acc)
    0 player.items

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
