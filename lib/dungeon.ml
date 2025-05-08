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
  mutable exits : room_exit list;
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
let min_rooms = 5
let max_rooms = 10
let min_exits = 1
let max_exits = 4

(* given a list of availible coordinates, a room (number in the list of rooms
   the dungeon holds), and a list of already placed exits in the room, get a
   Coordinate at which to place a new exit. *)
let rec place_exit (empty_coords : Coords.t list) to_room
    (existing_exits : room_exit list) : Coords.t =
  let coords = List.nth empty_coords (Random.int (List.length empty_coords)) in
  let x_coord = coords.x in
  let y_coord = coords.y in
  if
    List.mem
      { coords = { Coords.x = x_coord; Coords.y = y_coord }; to_room }
      existing_exits
  then place_exit empty_coords to_room existing_exits
  else coords

(* A connection between two rooms *)
type room_connection = {
  room1 : int;
  room2 : int;
}

(* frankly, i forgot what exactly this function does *)
let create_exits (room_sizes : (int * int) list) =
  (* Create a list to store all bi-directional connections *)
  let connections = ref [] in

  (* Connect each room to the next one to form a path *)
  for i = 0 to List.length room_sizes - 2 do
    connections := { room1 = i; room2 = i + 1 } :: !connections
  done;

  (* Add more random connections to make it interesting *)
  let num_extra =
    (List.length room_sizes / 3) + Random.int (List.length room_sizes)
  in
  for _ = 0 to num_extra do
    let r1 = Random.int (List.length room_sizes) in
    let r2 = Random.int (List.length room_sizes) in
    if r1 <> r2 then connections := { room1 = r1; room2 = r2 } :: !connections
  done;

  (* Convert connections to the rooms array format *)
  let rooms = Array.make (List.length room_sizes) [] in
  List.iter
    (fun conn ->
      rooms.(conn.room1) <- conn.room2 :: rooms.(conn.room1);
      rooms.(conn.room2) <- conn.room1 :: rooms.(conn.room2))
    !connections;

  (* Remove duplicate connections *)
  for i = 0 to Array.length rooms - 1 do
    rooms.(i) <- List.sort_uniq Int.compare rooms.(i)
  done;

  (rooms, !connections)

(* given a list of coordinates (1 / exit) and a list of the room that each exit
   should lead to, basically zip the 2 lists together to make a list of
   room_exit's*)
let create_exits_from_data coords_list to_room_list =
  Printf.printf "coords_list len: %i to_room_list len: %i\n"
    (List.length coords_list) (List.length to_room_list);

  (* Ensure we don't exceed the number of available coordinates *)
  let rec take n lst =
    match (n, lst) with
    | 0, _ -> []
    | _, [] -> []
    | n, h :: t -> h :: take (n - 1) t
  in

  (* if we have too many rooms to connect and not enough coordinates, only use
     as many room connections as we have coordinates available *)
  let usable_to_room_list = take (List.length coords_list) to_room_list in

  List.map2
    (fun coords to_room -> { coords; to_room })
    coords_list usable_to_room_list

(* function to actually randomly generate a dungeon *)
let generate () : t =
  let num_rooms = min_rooms + Random.int (max_rooms - min_rooms) in

  let room_data = List.init num_rooms (fun _ -> Room.pick_room_file ()) in

  let room_exits = List.map (fun (w, h, f) -> (w, h)) room_data in

  let exits, connections = create_exits room_exits in

  (* First create all the rooms with provisional exits *)
  let rooms = Array.make num_rooms { room = Room.new_room (); exits = [] } in
  for room_num = 0 to Array.length exits - 1 do
    let room, exit_coords =
      Room.generate (List.nth room_data room_num) (List.length exits.(room_num))
    in
    rooms.(room_num) <-
      { room; exits = create_exits_from_data exit_coords exits.(room_num) }
  done;

  (* Now ensure exits match up between paired rooms *)
  List.iter
    (fun conn ->
      (* Find exits connecting these rooms *)
      let r1_to_r2 =
        List.find
          (fun exit -> exit.to_room = conn.room2)
          rooms.(conn.room1).exits
      in
      let r2_to_r1 =
        List.find
          (fun exit -> exit.to_room = conn.room1)
          rooms.(conn.room2).exits
      in

      (* Ensure both exits point to each other *)
      rooms.(conn.room1).exits <-
        { coords = r1_to_r2.coords; to_room = conn.room2 }
        :: List.filter
             (fun e -> e.to_room <> conn.room2)
             rooms.(conn.room1).exits;

      rooms.(conn.room2).exits <-
        { coords = r2_to_r1.coords; to_room = conn.room1 }
        :: List.filter
             (fun e -> e.to_room <> conn.room1)
             rooms.(conn.room2).exits)
    connections;

  {
    rooms;
    player = Player.create ();
    hud_text = "Welcome. Press Enter to open command palette.";
    current_room = 0;
  }

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
    | Keyboard.Up | Keyboard.Down | Keyboard.Right | Keyboard.Left ->
        Coords.add_dir pos 1 dir
    | _ -> failwith "invalid keyboard input to calculate move with"
  in
  let matching_exits =
    List.filter
      (fun curr_exit -> Coords.equal curr_exit.coords pos_after_move)
      dungeon.rooms.(dungeon.current_room).exits
  in
  if List.length matching_exits = 1 then Some (List.hd matching_exits) else None
(* TODO add check if matching_exits len != 1? *)

let get_pos_in_new_room old_room_num new_room =
  (List.find (fun exit -> exit.to_room = old_room_num) new_room.exits).coords
(*TODO error handling, what if no list head, etc*)
(*TODO what if each room has multiple exits that connect? right now it just
  picks the first exit that matches *)

let player dungeon = dungeon.player

let move_player dungeon direction =
  let curr_room = dungeon.rooms.(dungeon.current_room) in
  match will_this_move_lead_to_an_exit dungeon direction with
  | None -> Room.move_player curr_room.room direction (player dungeon)
  | Some exit ->
      let old_room_num = dungeon.current_room in
      dungeon.current_room <- exit.to_room;
      Room.set_player_pos dungeon.rooms.(dungeon.current_room).room
        (get_pos_in_new_room old_room_num dungeon.rooms.(dungeon.current_room))
(* TODO add check that the new position is correct? *)

let hud_text dungeon = dungeon.hud_text
