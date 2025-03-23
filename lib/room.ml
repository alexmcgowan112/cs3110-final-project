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

let new_room () =
  { tiles = Array.make_matrix 11 11 Empty; playerLoc = { x = 5; y = 5 } }

let move_player room = function
  | Up ->
      if room.playerLoc.y > 0 then
        room.playerLoc <- { x = room.playerLoc.x; y = room.playerLoc.y - 1 }
  | Down ->
      if room.playerLoc.y < Array.length room.tiles - 1 then
        room.playerLoc <- { x = room.playerLoc.x; y = room.playerLoc.y + 1 }
  | Left ->
      if room.playerLoc.x > 0 then
        room.playerLoc <- { x = room.playerLoc.x - 1; y = room.playerLoc.y }
  | Right ->
      if room.playerLoc.x < Array.length room.tiles.(0) - 1 then
        room.playerLoc <- { x = room.playerLoc.x + 1; y = room.playerLoc.y }

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
