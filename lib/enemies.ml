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

open Coords

let neighbors room (c : Coords.t) =
  let deltas = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in
  let str_matrix = Room.to_string_matrix room in
  let max_y = Array.length str_matrix - 1 in
  let max_x = Array.length str_matrix.(0) - 1 in
  List.filter_map
    (fun (dx, dy) ->
      let nx, ny = (c.x + dx, c.y + dy) in
      if nx >= 0 && ny >= 0 && ny <= max_y && nx <= max_x then
        let s = str_matrix.(ny).(nx) in
        if s = "#" then None (* Wall *) else Some { x = nx; y = ny }
      else None)
    deltas

let find_path room start goal =
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
              let key = (n.x, n.y) in
              if not (Hashtbl.mem visited key) then begin
                Hashtbl.add visited key true;
                Hashtbl.add came_from key curr;
                Queue.add n q
              end)
            (neighbors room curr);
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

let next_move2 room this =
  let start = this.position in
  let goal = Room.get_player_pos room in
  let path = find_path room start goal in
  match path with
  | [] -> start
  | [ _ ] -> start
  | _ :: h :: _ -> h

let take_damage this damage =
  this.health <- this.health - damage;
  if this.health < 0 then this.health <- 0

let is_alive this = this.health > 0
