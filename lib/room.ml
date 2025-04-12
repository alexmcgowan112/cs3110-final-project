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
  mutable hud_text : string;
}
(** AF: [{tiles; playerLoc; explosion}] represents a room with [tiles], a player
    at [playerLoc] and a (potential) current explosion [explosion]. RI:
    [playerLoc] is in bounds and is not inside of a wall tile. [explosion] is
    None if no explosion is currently happening. *)

(* Utility Functions *)
let string_to_tile = function
  | "#" -> Wall
  | "O" -> Exit
  | _ -> Empty

let tile_to_string = function
  | Empty -> "."
  | Wall -> "#"
  | Exit -> "O"

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
    | None ->
        if
          List.exists
            (fun exp -> Explosion.tile_is_exploding coords exp)
            room.explosions
        then "*"
        else tile_to_string room.tiles.(coords.y).(coords.x)

let to_string_matrix room =
  Array.mapi
    (fun y row -> Array.mapi (fun x _ -> coords_to_string room { x; y }) row)
    room.tiles

let str_matrix_to_string mat =
  let row_to_string row =
    Array.fold_left (fun acc s -> if acc = "" then s else acc ^ " " ^ s) "" row
  in
  Array.fold_left
    (fun acc row ->
      if acc = "" then row_to_string row else acc ^ "\n" ^ row_to_string row)
    "" mat

let to_string room = room |> to_string_matrix |> str_matrix_to_string
let get_player_pos { playerLoc; _ } = playerLoc

(* Room Creation *)

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
  else { tiles; playerLoc; explosions = []; bombs = []; hud_text = "Welcome." }

let new_room () = load_room_from_file "data/rooms/simple.json"

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

let wait = process_bombs
let hud_text room = room.hud_text

let set_hud_text room text =
  room.hud_text <- text;
  let y, x = Curses.getyx (Curses.stdscr ()) in
  ignore (Curses.move y 0);
  Curses.clrtoeol ();
  ignore (Curses.addstr text);
  ignore (Curses.refresh ())

let set_player_pos room loc =
  room.playerLoc <- loc 
  (* TODO add check that new location is valid *)
