open Connections

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
  mutable enemies : Enemies.t option array;
  graph : G.t;
  mutable items : Item.t list;
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
  let lst = List.filter_map (fun e -> e) (Array.to_list room.enemies) in
  let enemies_at_loc =
    List.filter (fun enemy -> Enemies.get_position enemy = coords) lst
  in
  if List.length enemies_at_loc <> 0 then Some (List.hd enemies_at_loc)
  else None

(*[item_here room coords] is Some item if there's an item at the provided coords
  in this room and None otherwise*)
let item_here room coords =
  let items_at_loc =
    List.filter
      (fun item ->
        match Item.get_location item with
        | None -> false
        | Some loc -> loc = coords)
      room.items
  in
  if List.length items_at_loc <> 0 then Some (List.hd items_at_loc) else None

let coords_to_string room (coords : Coords.t) =
  if
    coords.y < 0 || coords.x < 0
    || coords.y > Array.length room.tiles
    || coords.x > Array.length room.tiles.(0)
  then "."
  else if Coords.equal room.playerLoc coords then "@"
  else
    let bomb =
      List.find_opt (fun bomb -> Coords.equal bomb.position coords) room.bombs
    in
    match bomb with
    | Some b -> string_of_int b.fuse
    | None -> (
        if Explosion.tile_is_exploding coords room.explosions room.graph then
          "*"
        else if enemy_here room coords <> None then "X"
        else
          match item_here room coords with
          | Some item -> Item.to_string item
          | None -> tile_to_string room.tiles.(coords.y).(coords.x))

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

let json_to_items lst =
  Yojson.Safe.Util.(
    lst |> to_list
    |> List.map (fun json_item ->
           match to_string (member "type" json_item) with
           | "item" ->
               Item.create_item 9999 Item.Item
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | "armor" ->
               Item.create_item 9999
                 (Item.Armor { def = ref (to_int (member "def" json_item)) })
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | "biggerRadius" ->
               Item.create_item 9999 Item.BiggerRadius
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | "shorterFuse" ->
               Item.create_item 9999 Item.ShorterFuse
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | "health" ->
               Item.create_item 9999 Item.Health
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | "bomb" ->
               Item.create_item 9999 Item.Bomb
                 (Some
                    {
                      x = to_int (member "x_coord" json_item);
                      y = to_int (member "y_coord" json_item);
                    })
           | _ -> failwith "invalid room json - unknown item type"))

(* the enemies in a room are determined by the room's JSON file. Enemies are
   stored as a list of dictionaries. Each dictionary contains start_x_coord and
   start_y_coord (and that's it for now)*)
let json_to_enemies lst =
  Yojson.Safe.Util.(
    lst |> to_list |> Array.of_list
    |> Array.map (fun json_enemy ->
           Some
             (Enemies.create
                {
                  x = to_int (member "start_x_coord" json_enemy);
                  y = to_int (member "start_y_coord" json_enemy);
                }
                (to_string (member "enemy_type" json_enemy)))))

let tiles_to_graph tiles =
  let g = G.create () in
  Array.iteri
    (fun y row ->
      Array.iteri
        (fun x tile ->
          if tile <> Wall then (
            let (coords : Coords.t) = { x; y } in
            G.add_vertex g coords;
            List.iter
              (fun (dx, dy) ->
                let (neighbor : Coords.t) = { x = x + dx; y = y + dy } in
                if
                  neighbor.x >= 0 && neighbor.y >= 0
                  && neighbor.x < Array.length row
                  && neighbor.y < Array.length tiles
                  && tiles.(neighbor.y).(neighbor.x) <> Wall
                then G.add_edge g coords neighbor)
              [ (-1, 0); (1, 0); (0, -1); (0, 1) ]))
        row)
    tiles;
  g

(* [load_room_from_file filename] loads a room from the given JSON file. *)
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
  let items = get_from_json "items" |> json_to_items in
  let tiles =
    Yojson.Safe.Util.to_string (get_from_json "layout") |> read_file_to_tiles
  in
  if Array.exists (fun row -> Array.length row <> Array.length tiles.(0)) tiles
  then failwith "Layout isn't rectangular"
  else
    {
      tiles;
      playerLoc;
      explosions = [];
      bombs = [];
      enemies;
      graph = tiles_to_graph tiles;
      items;
    }

let new_room () = load_room_from_file "data/rooms/test_rooms/simple.json"
let max_width = 30
let min_width = 10
let max_height = max_width
let min_height = min_width

(* overwrite all the exits in the provided tiles with the provided exits. if
   there's other exits not in exits_coords (but was defined in the actual room
   file *.rm), replace with a wall*)
let set_exits tiles exits_coords =
  Array.iteri
    (fun row_num row ->
      Array.iteri
        (fun col_num tile ->
          if List.mem { Coords.x = col_num; Coords.y = row_num } exits_coords
          then tiles.(row_num).(col_num) <- Exit
          else
            match tiles.(row_num).(col_num) with
            | Exit -> tiles.(row_num).(col_num) <- Wall
            | _ -> ())
        row)
    tiles

let min_enemies = 0
let max_enemies = 5

(* tile array array -> list of coords (tiles with nothing on them)*)
let empty_tiles (tiles : tile array array) =
  let empties_as_array =
    Array.mapi
      (fun row_num row ->
        Array.mapi
          (fun col_num col ->
            match col with
            | Empty -> Some { Coords.x = col_num; y = row_num }
            | _ -> None)
          row)
      tiles
  in
  let empties_nested_list =
    Array.fold_left (fun acc row -> acc @ Array.to_list row) [] empties_as_array
  in
  List.filter_map
    (fun c_opt ->
      match c_opt with
      | Some c -> Some c
      | None -> None)
    empties_nested_list

let generate_enemies tiles room_width room_height : Enemies.t option array =
  let num_enemies = min_enemies + Random.int (max_enemies - min_enemies + 1) in
  let valid_positions = empty_tiles tiles in
  if List.length valid_positions = 0 then [||]
  else
    Array.init num_enemies (fun _ ->
        let pos =
          List.nth valid_positions (Random.int (List.length valid_positions))
        in
        Some (Enemies.create pos Enemies.Ghost))
(* TODO make sure 2 enemies dont spawn on the same spot, make sure the spot isnt
   a wall, make sure its not where the player will be, etc *)

let generate_items (tiles : tile array array) =
  let min_items = 2 in
  let max_items = 3 in
  let num_items = min_items + Random.int (max_items - min_items + 1) in
  let valid_positions = empty_tiles tiles in
  let rec aux n acc used_positions =
    if n <= 0 then acc
    else
      let avail_positions =
        List.filter
          (fun pos ->
            not (List.exists (fun p -> Coords.equal p pos) used_positions))
          valid_positions
      in
      (* If there are no more available positions, stop generating items *)
      if avail_positions = [] then acc
      else
        let pos =
          List.nth avail_positions (Random.int (List.length avail_positions))
        in
        let stat =
          match Random.int 3 with
          | 0 -> Item.Armor { def = ref 2 }
          | 1 -> Item.BiggerRadius
          | 2 -> Item.ShorterFuse
          | _ -> failwith "a random int 0-2 was not 0-2 (should be impossible)"
        in
        let item = Item.create_item (1000 + n) stat (Some pos) in
        aux (n - 1) (item :: acc) (pos :: used_positions)
  in
  aux num_items [] []

let rooms_dir = "data/rooms/medium_dungeon"

(* pick a random room (\*.rm) file to use for this particular room *)
let pick_room_file () =
  let room_files =
    Array.fold_left
      (fun acc file ->
        if Filename.check_suffix file "rm" then (rooms_dir ^ "/" ^ file) :: acc
        else acc) (*TODO is the / above cross platform?*)
      [] (Sys.readdir rooms_dir)
    (*TODO make sure to keep the rooms_dir up to date if we move files around*)
  in
  let room_file = List.nth room_files (Random.int (List.length room_files)) in
  let tiles = read_file_to_tiles room_file in
  let width = Array.length tiles in
  let height = Array.length tiles.(0) in
  (width, height, room_file)

let get_empty_tiles room = empty_tiles room.tiles

(* given tiles and a number of exits to create, return a list of Coords for each
   exit *)
let rec create_exits tiles num_exits =
  let exit_options = empty_tiles tiles in
  let rec aux acc =
    if List.length acc >= num_exits then acc
    else
      let coord =
        List.nth exit_options (Random.int (List.length exit_options))
      in
      let acc' = if List.mem coord acc then acc else coord :: acc in
      aux acc'
  in
  aux []

let generate (width, height, room_file) num_exits : t * Coords.t list =
  let tiles = read_file_to_tiles room_file in
  let exits_coords = create_exits tiles num_exits in
  let () = set_exits tiles exits_coords in
  let enemies = generate_enemies tiles width height in
  let items = generate_items tiles in
  let graph = tiles_to_graph tiles in
  let playerLoc_options = empty_tiles tiles in
  let playerLoc =
    List.nth playerLoc_options (Random.int (List.length playerLoc_options))
  in
  ( { tiles; enemies; items; graph; bombs = []; explosions = []; playerLoc },
    exits_coords )

let get_enemies room = room.enemies

(* Game Logic *)
let explode room =
  List.iter Explosion.spread room.explosions;
  room.explosions <- List.filter Explosion.is_in_progress room.explosions

let exploding room = not (List.is_empty room.explosions)

let place_bomb room player =
  if Player.bombs player <= 0 then ()
  else if
    (not
       (List.exists
          (fun bomb -> Coords.equal room.playerLoc bomb.position)
          room.bombs))
    && Player.bombs player > 0
  then (
    Player.remove_bombs player 1;
    room.bombs <-
      { position = room.playerLoc; fuse = Player.fuse_time player }
      :: room.bombs)

let process_bombs room player =
  List.iter
    (fun b ->
      b.fuse <- b.fuse - 1;
      if b.fuse = 0 then
        room.explosions <-
          Explosion.create b.position (Player.blast_radius player)
          :: room.explosions)
    room.bombs;
  room.bombs <- List.filter (fun b -> b.fuse > 0) room.bombs

let move_player room direction player =
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
  process_bombs room player

let remove_explosion_tiles room enemy =
  let new_graph = G.copy room.graph in
  List.iter
    (G.remove_vertex new_graph)
    (List.map (fun b -> b.position) room.bombs);
  G.iter_vertex
    (fun v ->
      if Explosion.tile_is_exploding v room.explosions room.graph then
        G.remove_vertex new_graph v)
    new_graph;
  new_graph

let add_explosion_to_room room exp = room.explosions <- exp :: room.explosions


let update_enemy room player e =
  match e with
  | None -> None
  | Some enemy ->
      let new_graph = remove_explosion_tiles room e in
      if
        Explosion.tile_is_exploding
          (Enemies.get_position enemy)
          room.explosions room.graph
      then Enemies.take_damage enemy 1 (add_explosion_to_room room)
      else
        Some
          (Enemies.move_or_attack enemy room.playerLoc player new_graph
             room.enemies (add_explosion_to_room room))

let update_enemies room player =
  Array.map_inplace (update_enemy room player) room.enemies

(**[remove_from_room room item] removes the item from the room*)
let remove_from_room room item =
  room.items <- List.filter (fun curr_item -> curr_item <> item) room.items

let update_items room player =
  match item_here room room.playerLoc with
  | None -> ()
  | Some item ->
      remove_from_room room item;
      Item.pickup_item item;
      Player.equip player item

let update_items room player =
  match item_here room room.playerLoc with
  | None -> ()
  | Some item ->
      remove_from_room room item;
      Item.pickup_item item;
      Player.equip player item

let wait = process_bombs
let set_player_pos room loc = room.playerLoc <- loc
(* TODO add check that new location is valid *)

let update_player_health room player =
  if
    Explosion.tile_is_exploding (get_player_pos room) room.explosions room.graph
  then Player.damage player 1 (*explosion damage*)
