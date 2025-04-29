(* my plan for making Player.ml: the Player is basically just a list of stats.
   Its location is managed by the Dungeon/Room. Subject to change. *)
type t = { mutable health : int }

let health player = player.health
let create () = { health = 5 }
let damage player dmg_amount = player.health <- player.health - dmg_amount
let is_alive player = player.health > 0
