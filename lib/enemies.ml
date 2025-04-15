type t = {
  id : int;
  mutable position : Coords.t;
  mutable health : int;
}

let create coords id = { id; position = coords; health = 100 }
let move this coords = this.position <- coords
let get_position this = this.position

let next_move room this =
  (* Placeholder logic for next move. Replace an A* closest path algorithm. This
     does not yet guard against walls lol*)
  let target = Room.get_player_pos room in
  let dx =
    if target.Coords.x > this.position.Coords.x then 1
    else if target.Coords.x < this.position.Coords.x then -1
    else 0
  in
  let dy =
    if target.Coords.y > this.position.Coords.y then 1
    else if target.Coords.y < this.position.Coords.y then -1
    else 0
  in
  {
    Coords.x = this.position.Coords.x + dx;
    Coords.y = this.position.Coords.y + dy;
  }

let take_damage this damage =
  this.health <- this.health - damage;
  if this.health < 0 then this.health <- 0

let is_alive this = this.health > 0
