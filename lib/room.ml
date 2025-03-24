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

type t = {
  tiles : tile array array;
  mutable playerLoc : coords;
}
(** AF: [{tiles; playerLoc}] represents a room with [tiles] and a player at
    [playerLoc]. RI: [playerLoc] is in bounds and is not inside of a wall tile.
*)

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
  { tiles; playerLoc = { x = 5; y = 5 } }

let move_player room direction =
  let { x; y } = room.playerLoc in
  match direction with
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
      then room.playerLoc <- { x = x + 1; y }

let tile_to_string = function
  | Empty -> "_"
  | Wall -> "#"
  | Exit -> "O"
  | Item i -> "!"

let to_string room =
  let tiles = Array.map (fun row -> Array.map tile_to_string row) room.tiles in
  tiles.(room.playerLoc.y).(room.playerLoc.x) <- "@";
  let row_to_string row =
    Array.fold_left (fun acc s -> if acc = "" then s else acc ^ " " ^ s) "" row
  in
  Array.fold_left
    (fun acc row ->
      if acc = "" then row_to_string row else acc ^ "\n" ^ row_to_string row)
    "" tiles

let get_player_pos { playerLoc; _ } = playerLoc
