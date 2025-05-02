open Connections

type enemy_type = Ghost

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
  enemy_type : enemy_type; (* The type of enemy. Currently only Ghost. *)
}
(** AF: [{position; health; atk_range; atk_damage; can_act}] represents an enemy
    current at [position], with [health]. The enemy can hit the player from a
    distance of [atk_range] and does [atk_damage] when they hit the player.
    [can_act] determines if the enemy is able to act on the next timestep. *)

(*ghost uses next_move. goes thorugh walls zombie uses ____. does not go through
  walls*)

let create coords enemy_type =
  match enemy_type with
  | Ghost ->
      (* Ghosts can go through walls *)
      {
        position = coords;
        health = 100;
        atk_range = 1;
        atk_damage = 1;
        can_act = true;
        enemy_type = Ghost;
      }

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

(** [bfs_next_step graph start goal all_enemies] figures out a move from [start]
    in [graph] towards [goal] based on [all_enemies] *)
let bfs_next_step room_graph start goal all_enemies =
  if start = goal then None
  else
    let pred = Hashtbl.create 16 in
    let visited = Hashtbl.create 16 in
    let queue = Queue.create () in

    Queue.push start queue;
    Hashtbl.add visited start ();

    let found = ref false in

    while (not (Queue.is_empty queue)) && not !found do
      let v = Queue.pop queue in
      G.iter_succ
        (fun u ->
          if not (Hashtbl.mem visited u) then begin
            Hashtbl.add visited u ();
            Hashtbl.add pred u v;
            Queue.push u queue;
            if u = goal then found := true
          end)
        room_graph v
    done;

    if not (Hashtbl.mem pred goal) then None
    else
      let rec backtrack current =
        let parent = Hashtbl.find pred current in
        if parent = start then Some current else backtrack parent
      in
      match backtrack goal with
      | Some step -> if enemy_at_pos step all_enemies then None else Some step
      | None -> None

let random_next_move graph curr all_enemies =
  let neighbors = G.succ graph curr in
  match neighbors with
  | [] -> curr
  | _ ->
      let len = List.length neighbors in
      let idx = Random.int len in
      let att = List.nth neighbors idx in
      if
        Array.mem att
          (Array.map
             (fun e ->
               match e with
               | None -> curr
               | Some p -> get_position p)
             all_enemies)
      then curr
      else att

let next_move target this room_graph all_enemies =
  let curr = get_position this in
  let potential = bfs_next_step room_graph curr target all_enemies in
  match potential with
  | None -> random_next_move room_graph curr all_enemies
  | Some next -> next

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

let take_damage this damage =
  this.health <- this.health - damage;
  if this.health < 0 then this.health <- 0

let is_alive this = this.health > 0

let move_or_attack enemy player_loc player room_graph all_enemies =
  if enemy.can_act then
    if Coords.manhattan_dist enemy.position player_loc <= enemy.atk_range then
      Player.damage player enemy.atk_damage
    else
      move enemy
        (match enemy.enemy_type with
        | Ghost -> next_move player_loc enemy room_graph all_enemies);
  enemy.can_act <- not enemy.can_act;
  (* enemy can do something every other turn (to make it easier for the
     player)*)
  enemy
