type coords = {
  x : int;
  y : int;
}

type direction =
  | Up
  | Down
  | Left
  | Right

type item =
  | Placeholder1
  | Placeholder2

type tile =
  | Empty
  | Wall
  | Exit
  | Item of item
  | Explosion of tile

type explosion = {
  radius : int;
  center_x : int;
  center_y : int;
  mutable curr_radius : int;
}

type bomb = {
  pos : coords;
  mutable fuse : int;
}

type t = {
  mutable tiles : tile array array;
  mutable playerLoc : coords;
  mutable explosion : explosion option;
  mutable bombs : bomb list;
}

(** AF: [{tiles; playerLoc; explosion}] represents a room with [tiles], a player
    at [playerLoc] and a (potential) current explosion [explosion]. RI:
    [playerLoc] is in bounds and is not inside of a wall tile. [explosion] is
    None if no explosion is currently happening. *)

let new_room () =
  let tiles = Array.make_matrix 11 11 Empty in
  List.iter
    (fun (x, y) -> tiles.(y).(x) <- Wall)
    [
      (2, 4);
      (2, 5);
      (2, 6);
      (8, 4);
      (8, 5);
      (8, 6);
      (4, 2);
      (5, 2);
      (6, 2);
      (4, 8);
      (5, 8);
      (6, 8);
    ];
  { tiles; playerLoc = { x = 5; y = 5 }; explosion = None; bombs = [] }

(* open Curses *)
(* let red_text s = "\027[31m" ^ s ^ "\027[0m" *)

let tile_to_string = function
  | Empty -> "_"
  | Wall -> "#"
  | Exit -> "O"
  | Item i -> "!"
  | Explosion _ -> "*"
(* to make the explosion red, from ansiterminal. see
   https://github.com/Chris00/ANSITerminal/blob/master/src/ANSITerminal_unix.ml*)

let to_string room =
  let tiles = Array.map (fun row -> Array.map tile_to_string row) room.tiles in
  List.iter
    (fun { pos = { x; y }; fuse } -> tiles.(y).(x) <- string_of_int fuse)
    room.bombs;
  tiles.(room.playerLoc.y).(room.playerLoc.x) <- "@";
  let row_to_string row =
    Array.fold_left (fun acc s -> if acc = "" then s else acc ^ " " ^ s) "" row
  in
  Array.fold_left
    (fun acc row ->
      if acc = "" then row_to_string row else acc ^ "\n" ^ row_to_string row)
    "" tiles

let to_array room = room.tiles
let get_player_pos { playerLoc; _ } = playerLoc

let tile_is_exploding start_x start_y tile_x tile_y radius =
  (* manhatten distance *)
  abs (start_x - tile_x) + abs (start_y - tile_y) = radius

let update_explosion x y radius room =
  Array.iteri
    (fun curr_row_num curr_row ->
      Array.iteri
        (fun curr_col_num curr_tile ->
          if tile_is_exploding x y curr_row_num curr_col_num radius then
            room.(curr_row_num).(curr_col_num) <- Explosion curr_tile
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
      update_explosion explosion.center_x explosion.center_y
        explosion.curr_radius room.tiles;
      if explosion.curr_radius <= explosion.radius then
        explosion.curr_radius <- explosion.curr_radius + 1
      else clear_explosion room
  | None -> ()

let start_exploding room center_x center_y radius =
  room.explosion <- Some { radius; curr_radius = 0; center_x; center_y }

let exploding room = room.explosion <> None

let place_bomb room =
  room.bombs <- { pos = room.playerLoc; fuse = 5 } :: room.bombs

let move_player room direction =
  let { x; y } = room.playerLoc in
  (match direction with
  | Up ->
      if y > 0 && room.tiles.(y - 1).(x) <> Wall then
        room.playerLoc <- { x; y = y - 1 }
  | Down ->
      if y < Array.length room.tiles - 1 && room.tiles.(y + 1).(x) <> Wall then
        room.playerLoc <- { x; y = y + 1 }
  | Left ->
      if x > 0 && room.tiles.(y).(x - 1) <> Wall then
        room.playerLoc <- { x = x - 1; y }
  | Right ->
      if x < Array.length room.tiles.(0) - 1 && room.tiles.(y).(x + 1) <> Wall
      then room.playerLoc <- { x = x + 1; y });
  List.iter
    (fun b ->
      b.fuse <- b.fuse - 1;
      if b.fuse = 0 then start_exploding room b.pos.y b.pos.x 3)
    room.bombs;
  room.bombs <- List.filter (fun b -> b.fuse > 0) room.bombs
