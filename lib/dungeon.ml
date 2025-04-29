(* A [room_exit] is the coordinates of the exit inside the [dungeon_room] it is
   attached to and the index in the dungeon's list of rooms (i.e. the room) that
   this exit leads to *)
type room_exit = {
  coords : Coords.t;
  to_room : int;
}

(* [dungeon_room] is a room inside a dungeon. It consists of the [Room] object
   and a list of exits for that room. *)
type dungeon_room = {
  room : Room.t;
  (* exits: dict of exits for a room *)
  exits : room_exit list;
}

(* TODO AF, RI *)
(* A [dungeon] is stored as a list of [dungeon_rooms] and the index 
of the room that the player is currently in in the dungeon's rooms list.
There is also a [Player] object that is basically a list of stats for the player (currently just the player's health) *)
type t = {
  rooms : dungeon_room Array.t;
  player : Player.t;
  mutable current_room : int;
  mutable hud_text : string;
}

(* How are dungeons stored right now? *)
(* A dungeon is saved as a JSON file in data/dungeons.  *)
(* The JSON file has 2 elements: rooms (a list), and current_room (an int) *)
(* Rooms is a list of dictionaries. Each dictionary represents a dungeon_room.  *)
(* Inside the dungeon_room dictionary, there are 2 parts: room_file (string) and exits (dict. list) *)
(* Room_file is a string corresponding to the JSON file containing the room data. Exits is a dictionary list of exits for the room.  *)
(* An Exit contains the x and y coord of the exit and the index of the room in the list that the exit leads to  *)
(* (i.e. if, for a given exit, to_room is 0, then this exit connects to the first room in the dungeon room list) *)
(* This is so the player can see where the exits are. This, and the whole dungeon storage structure, can be improved. *)
let get_from_json filename key =
  Yojson.Safe.from_file filename |> Yojson.Safe.Util.member key

let json_to_dungeon_rooms lst : dungeon_room Array.t =
  Yojson.Safe.Util.(
    lst |> to_list
    |> List.map (fun json_room ->
           let room =
             Room.load_room_from_file (to_string (member "room_file" json_room))
           in
           let exits =
             List.map
               (fun json_exit ->
                 {
                   coords =
                     {
                       x = to_int (member "x_coord" json_exit);
                       y = to_int (member "y_coord" json_exit);
                     };
                   to_room = to_int (member "to_room" json_exit);
                 })
               (to_list (member "exits" json_room))
           in
           { room; exits })
    |> Array.of_list)

let load_dungeon_from_file filename =
  let get_int_from_json key =
    get_from_json filename key |> Yojson.Safe.Util.to_int
  in
  let current_room = get_int_from_json "current_room" in
  let rooms = get_from_json filename "rooms" |> json_to_dungeon_rooms in
  let player = Player.create () in
  {
    rooms;
    player;
    current_room;
    hud_text = "Welcome. Press Enter to open command palette.";
  }

let create () = load_dungeon_from_file "data/dungeons/simple.json"
let create_test () = load_dungeon_from_file "../data/dungeons/test.json"
let current_room dungeon = dungeon.rooms.(dungeon.current_room).room

let set_hud_text dungeon text =
  dungeon.hud_text <- text;
  let y, x = Curses.getyx (Curses.stdscr ()) in
  ignore (Curses.move y 0);
  Curses.clrtoeol ();
  ignore (Curses.addstr text);
  ignore (Curses.refresh ())

(*TODO make sure the location after the move is still in bounds of room*)
(* For a given room and a direction that the player will move in, this function 
checks the player's position in the room and calculates if this move will put 
  the player on an exit (i.e. if the dungeon will need to switch the player 
    to a new room). If this move leads to an exit, the function returns an 
option with the corresponding exit in the new room. If not, it returns None. *)
let will_this_move_lead_to_an_exit dungeon dir =
  let pos = Room.get_player_pos (current_room dungeon) in
  let (pos_after_move : Coords.t) =
    match dir with
    | Keyboard.Up -> { x = pos.x; y = pos.y - 1 }
    | Keyboard.Down -> { x = pos.x; y = pos.y + 1 }
    | Keyboard.Right -> { x = pos.x + 1; y = pos.y }
    | Keyboard.Left -> { x = pos.x - 1; y = pos.y }
    | _ -> failwith "invalid keyboard input to calculate move with"
  in
  let matching_exits =
    List.filter
      (fun curr_exit -> curr_exit.coords = pos_after_move)
      dungeon.rooms.(dungeon.current_room).exits
  in
  if List.length matching_exits = 1 then Some (List.hd matching_exits) else None
(* TODO add check if matching_exits len != 1? *)

let get_pos_in_new_room old_room_num new_room =
  (List.hd
     (List.filter (fun exit -> exit.to_room = old_room_num) new_room.exits))
    .coords
(*TODO error handling, what if no list head, etc*)
(*TODO what if each room has multiple exits that connect? right now it just
  picks the first exit that matches *)

let move_player dungeon direction =
  let curr_room = dungeon.rooms.(dungeon.current_room) in
  match will_this_move_lead_to_an_exit dungeon direction with
  | None -> Room.move_player curr_room.room direction
  | Some exit ->
      let old_room_num = dungeon.current_room in
      dungeon.current_room <- exit.to_room;
      Room.set_player_pos dungeon.rooms.(dungeon.current_room).room
        (get_pos_in_new_room old_room_num dungeon.rooms.(dungeon.current_room))
(* TODO add check that the new position is correct? *)

let hud_text dungeon = dungeon.hud_text
let player dungeon = dungeon.player
