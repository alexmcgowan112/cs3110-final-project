type tile =
  | Empty
  | Wall
  | Exit

type bomb = {
  position : Coords.t;
  mutable fuse : int;
}

type t = {
  mutable tiles : tile array array;
  mutable playerLoc : Coords.t;
  mutable explosions : Explosion.t list;
  mutable bombs : bomb list;
  mutable enemies : Enemies.t list;
}
(** AF: [{tiles; playerLoc; explosions; bombs}] represents a room with [tiles],
    a player at [playerLoc] and a list current explosions [explosions], and
    bombs that haven't exploded yet [bombs]. RI: [playerLoc] is in bounds and is
    not inside of a wall tile. *)

(* Utility Functions *)
let char_to_tile = function
  | '#' -> Wall
  | 'O' -> Exit
  | _ -> Empty

let tile_to_string = function
  | Empty -> "."
  | Wall -> "#"
  | Exit -> "O"

(* [enemy_here room coords] is whether or not the given room contains an enemy
   at the given coordinates *)
let enemy_here room coords =
  let enemies_at_loc =
    List.filter (fun enemy -> Enemies.get_position enemy = coords) room.enemies
  in
  if List.length enemies_at_loc <> 0 then Some (List.hd enemies_at_loc)
  else None

let coords_to_string room (coords : Coords.t) =
  if
    coords.y < 0 || coords.x < 0
    || coords.y > Array.length room.tiles
    || coords.x > Array.length room.tiles.(0)
  then "."
  else if Coords.equal room.playerLoc coords then "@"
  else if enemy_here room coords <> None then "X"
    (* enemies currently only represented as Xs*)
  else
    let bomb =
      List.find_opt (fun bomb -> Coords.equal bomb.position coords) room.bombs
    in
    match bomb with
    | Some b -> string_of_int b.fuse
    | None ->
        if Explosion.tile_is_exploding coords room.explosions then "*"
        else tile_to_string room.tiles.(coords.y).(coords.x)

let to_string_matrix room =
  Array.mapi
    (fun y row -> Array.mapi (fun x _ -> coords_to_string room { x; y }) row)
    room.tiles

let get_player_pos { playerLoc; _ } = playerLoc

(* Room Creation *)
let read_file_to_tiles file =
  Array.map
    (fun s ->
      Array.init (String.length s) (fun i -> char_to_tile (String.get s i)))
    (BatArray.of_enum (BatFile.lines_of file))

(* the enemies in a room are determined by the room's JSON file. Enemies are
   stored as a list of dictionaries. Each dictionary contains start_x_coord and
   start_y_coord (and that's it for now)*)
let json_to_enemies lst =
  Yojson.Safe.Util.(
    lst |> to_list
    |> List.map (fun json_enemy ->
           Enemies.create
             {
               x = to_int (member "start_x_coord" json_enemy);
               y = to_int (member "start_y_coord" json_enemy);
             }))

let load_room_from_file filename =
  let get_from_json key =
    Yojson.Safe.from_file filename |> Yojson.Safe.Util.member key
  in
  let get_int_from_json key = get_from_json key |> Yojson.Safe.Util.to_int in
  let playerLoc : Coords.t =
    {
      x = get_int_from_json "playerStartX";
      y = get_int_from_json "playerStartY";
    }
  in
  let enemies = get_from_json "enemies" |> json_to_enemies in
  let tiles =
    Yojson.Safe.Util.to_string (get_from_json "layout") |> read_file_to_tiles
  in
  if Array.exists (fun row -> Array.length row <> Array.length tiles.(0)) tiles
  then failwith "Layout isn't rectangular"
  else { tiles; playerLoc; explosions = []; bombs = []; enemies }

let new_room () = load_room_from_file "data/rooms/test_rooms/simple.json"
let get_enemies room = room.enemies

(* Game Logic *)
let explode room =
  List.iter Explosion.spread room.explosions;
  room.explosions <- List.filter Explosion.is_in_progress room.explosions

let exploding room = not (List.is_empty room.explosions)

let place_bomb room =
  if
    not
      (List.exists
         (fun bomb -> Coords.equal room.playerLoc bomb.position)
         room.bombs)
  then room.bombs <- { position = room.playerLoc; fuse = 6 } :: room.bombs

let process_bombs room =
  List.iter
    (fun b ->
      b.fuse <- b.fuse - 1;
      if b.fuse = 0 then
        room.explosions <- Explosion.create b.position 3 :: room.explosions)
    room.bombs;
  room.bombs <- List.filter (fun b -> b.fuse > 0) room.bombs

let move_player room direction =
  let x, y = (room.playerLoc.x, room.playerLoc.y) in
  (match direction with
  | Keyboard.Up ->
      if y > 0 && room.tiles.(y - 1).(x) <> Wall then
        room.playerLoc <- { x; y = y - 1 }
  | Keyboard.Down ->
      if y < Array.length room.tiles - 1 && room.tiles.(y + 1).(x) <> Wall then
        room.playerLoc <- { x; y = y + 1 }
  | Keyboard.Left ->
      if x > 0 && room.tiles.(y).(x - 1) <> Wall then
        room.playerLoc <- { x = x - 1; y }
  | Keyboard.Right ->
      if x < Array.length room.tiles.(0) - 1 && room.tiles.(y).(x + 1) <> Wall
      then room.playerLoc <- { x = x + 1; y }
  | _ -> ());
  process_bombs room

let update_enemy room player enemy =
  if Explosion.tile_is_exploding (Enemies.get_position enemy) room.explosions
  then None (* right now, enemies insta-die when an explosion hits them. *)
  else Some (Enemies.move_or_attack enemy room.playerLoc player room.enemies)

let update_enemies room player =
  room.enemies <- List.filter_map (update_enemy room player) room.enemies

let wait = process_bombs
let set_player_pos room loc = room.playerLoc <- loc
(* TODO add check that new location is valid *)
