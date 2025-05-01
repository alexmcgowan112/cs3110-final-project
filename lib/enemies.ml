type t = {
  mutable position : Coords.t;
  mutable health : int;
  atk_range : int;
      (* from how many spaces / characters away the enemy can attack the player.
         Currently always 1 (must be right next to the player). *)
  atk_damage : int;
      (* how much damage the enemy does in a single attack. Currently always
         1. *)
  mutable can_act : bool;
      (* Tells if this enemy can make a move this turn. Currently, enemies can
         only act every other turn to make it easier for the player. *)
}
(** AF: [{position; health; atk_range; atk_damage; can_act}] represents an enemy
    current at [position], with [health]. The enemy can hit the player from a
    distance of [atk_range] and does [atk_damage] when they hit the player.
    [can_act] determines if the enemy is able to act on the next timestep. *)

let create coords =
  {
    position = coords;
    health = 100;
    atk_range = 1;
    atk_damage = 1;
    can_act = true;
  }

let attack enemy = enemy.atk_damage
let move this coords = this.position <- coords
let get_position this = this.position

(** [enemy_at_pos pos enemies] returns whether an enemy in [enemies] is present
    at [pos] *)
let enemy_at_pos coords enemies =
  Array.exists
    (fun enemy ->
      match enemy with
      | None -> false
      | Some e -> e.position = coords)
    enemies

let next_move target this all_enemies =
  (* Placeholder logic for next move. Replace an A* closest path algorithm. This
     does not yet guard against walls lol*)
  (* TODO enemies will not move to a space that already has an enemy in it, 
  but 2 enemies can move to the same space at the same time. Probably fix.*)
  let dx =
    if
      target.Coords.x > this.position.Coords.x
      && not
           (enemy_at_pos
              { x = this.position.Coords.x + 1; y = this.position.Coords.y }
              all_enemies)
    then 1
    else if
      target.Coords.x < this.position.Coords.x
      && not
           (enemy_at_pos
              { x = this.position.Coords.x - 1; y = this.position.Coords.y }
              all_enemies)
    then -1
    else 0
  in
  let dy =
    if
      target.Coords.y > this.position.Coords.y
      && not
           (enemy_at_pos
              { x = this.position.Coords.x; y = this.position.Coords.y + 1 }
              all_enemies)
    then 1
    else if
      target.Coords.y < this.position.Coords.y
      && not
           (enemy_at_pos
              { x = this.position.Coords.x; y = this.position.Coords.y - 1 }
              all_enemies)
    then -1
    else 0
  in
  {
    Coords.x = this.position.Coords.x + dx;
    Coords.y = this.position.Coords.y + dy;
  }

(* I updated this file to not take the rooms directly and instead just take
   whatever part they need (ie. player coords, string representation, etc) to
   avoid a circular import error. *)
let neighbors str_matrix (c : Coords.t) =
  let deltas = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
  let max_y = Array.length str_matrix - 1 in
  let max_x = Array.length str_matrix.(0) - 1 in
  List.filter_map
    (fun (dx, dy) ->
      let nx, ny = (c.x + dx, c.y + dy) in
      if nx >= 0 && ny >= 0 && ny <= max_y && nx <= max_x then
        let s = str_matrix.(ny).(nx) in
        if s = "#" then None
        (* Wall *) else Some { Coords.x = nx; Coords.y = ny }
      else None)
    deltas

let find_path room_str start goal =
  if Coords.equal start goal then [ start ]
  else
    let open Queue in
    let q = Queue.create () in
    let came_from = Hashtbl.create 100 in
    let visited = Hashtbl.create 100 in
    Queue.add start q;
    Hashtbl.add visited (start.x, start.y) true;
    let rec search () =
      if Queue.is_empty q then None
      else
        let curr = Queue.take q in
        if Coords.equal curr goal then Some curr
        else begin
          List.iter
            (fun n ->
              let key = (n.Coords.x, n.Coords.y) in
              if not (Hashtbl.mem visited key) then begin
                Hashtbl.add visited key true;
                Hashtbl.add came_from key curr;
                Queue.add n q
              end)
            (neighbors room_str curr);
          search ()
        end
    in
    match search () with
    | None -> []
    | Some _ ->
        let rec backtrack curr acc =
          if Coords.equal curr start then List.rev (curr :: acc)
          else
            let prev = Hashtbl.find came_from (curr.x, curr.y) in
            backtrack prev (curr :: acc)
        in
        backtrack goal []

let next_move2 goal room_str this =
  let start = this.position in
  let path = find_path room_str start goal in
  match path with
  | [] -> start
  | [ _ ] -> start
  | _ :: h :: _ -> h

let take_damage this damage =
  this.health <- this.health - damage;
  if this.health < 0 then this.health <- 0

let is_alive this = this.health > 0

let move_or_attack enemy player_loc player all_enemies =
  if enemy.can_act then
    if Coords.chebyshev_dist enemy.position player_loc <= enemy.atk_range then
      Player.damage player enemy.atk_damage
    else move enemy (next_move player_loc enemy all_enemies);
  enemy.can_act <- not enemy.can_act;
  (* enemy can do something every other turn (to make it easier for the
     player)*)
  enemy
