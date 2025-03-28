type tile =
  | Empty
  | Wall
  | Exit
  | Explosion of tile

type explosion = {
  radius : int;
  position : Coords.t;
  mutable curr_radius : int;
}

type bomb = {
  pos : Coords.t;
  mutable fuse : int;
}

type t = {
  mutable tiles : tile array array;
  mutable playerLoc : Coords.t;
  mutable explosion : explosion option;
  mutable bombs : bomb list;
}
(** AF: [{tiles; playerLoc; explosion}] represents a room with [tiles], a player
    at [playerLoc] and a (potential) current explosion [explosion]. RI:
    [playerLoc] is in bounds and is not inside of a wall tile. [explosion] is
    None if no explosion is currently happening. *)

let string_to_tile = function
  | "#" -> Wall
  | _ -> Empty

(** [json_to_tiles lst] turns a JSON string array array into an OCaml tile array
    array using [string_to_tile]. *)
let json_to_tiles lst =
  lst |> Yojson.Safe.Util.to_list
  |> List.map (fun row ->
         row |> Yojson.Safe.Util.to_list
         |> List.map (fun s -> Yojson.Safe.Util.to_string s |> string_to_tile)
         |> Array.of_list)
  |> Array.of_list

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
  let tiles = get_from_json "layout" |> json_to_tiles in
  if Array.exists (fun row -> Array.length row <> Array.length tiles.(0)) tiles
  then failwith "Layout isn't rectangular"
  else { tiles; playerLoc; explosion = None; bombs = [] }

let new_room () = load_room_from_file "data/rooms/simple.json"

let tile_to_string = function
  | Empty -> "."
  | Wall -> "#"
  | Exit -> "O"
  | Explosion _ -> "*"

let to_string_matrix room =
  let tiles = Array.map (fun row -> Array.map tile_to_string row) room.tiles in
  List.iter
    (fun { pos = { x; y }; fuse } -> tiles.(y).(x) <- string_of_int fuse)
    room.bombs;
  tiles.(room.playerLoc.y).(room.playerLoc.x) <- "@";
  tiles

let str_matrix_to_string mat =
  let row_to_string row =
    Array.fold_left (fun acc s -> if acc = "" then s else acc ^ " " ^ s) "" row
  in
  Array.fold_left
    (fun acc row ->
      if acc = "" then row_to_string row else acc ^ "\n" ^ row_to_string row)
    "" mat

let to_string room = room |> to_string_matrix |> str_matrix_to_string

(* let to_matrix room = Array.map Array.copy room.tiles *)
let get_player_pos { playerLoc; _ } = playerLoc

let tile_is_exploding exp_coords tile_coords radius =
  (* euclidian distance *)
  let distance = Coords.euclid_dist exp_coords tile_coords in
  radius = int_of_float (ceil (distance -. 0.5))

let update_explosion coords radius room =
  Array.iteri
    (fun curr_row_num curr_row ->
      Array.iteri
        (fun curr_col_num curr_tile ->
          if
            tile_is_exploding coords
              { x = curr_col_num; y = curr_row_num }
              radius
          then room.(curr_row_num).(curr_col_num) <- Explosion curr_tile
          else ())
        curr_row)
    room

let clear_explosion room =
  Array.iteri
    (fun curr_row_num curr_row ->
      Array.iteri
        (fun curr_col_num curr_tile ->
          match curr_tile with
          | Explosion old_tile ->
              room.tiles.(curr_row_num).(curr_col_num) <- old_tile
          | _ -> ())
        curr_row)
    room.tiles;
  room.explosion <- None

let explode room =
  match room.explosion with
  | Some explosion ->
      update_explosion explosion.position explosion.curr_radius room.tiles;
      if explosion.curr_radius <= explosion.radius then
        explosion.curr_radius <- explosion.curr_radius + 1
      else clear_explosion room
  | None -> ()

let start_exploding room position radius =
  room.explosion <- Some { radius; curr_radius = 0; position }

let exploding room = room.explosion <> None

let place_bomb room =
  room.bombs <- { pos = room.playerLoc; fuse = 6 } :: room.bombs

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
  List.iter
    (fun b ->
      b.fuse <- b.fuse - 1;
      if b.fuse = 0 then start_exploding room b.pos 3)
    room.bombs;
  room.bombs <- List.filter (fun b -> b.fuse > 0) room.bombs
