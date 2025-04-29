module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player dungeon dir = Dungeon.move_player dungeon dir

let read_string () =
  let rec loop buffer =
    let ch = Curses.getch () in
    match ch with
    | 10 -> buffer (* Enter key pressed, return the input *)
    | 127 | 8 ->
        (* Handle backspace *)
        let len = String.length buffer in
        if len > 0 then loop (String.sub buffer 0 (len - 1)) else loop buffer
    | _ -> loop (buffer ^ String.make 1 (Char.chr ch))
  in
  loop ""

let command_palette ?(test_input_string = "") room =
  Dungeon.set_hud_text room "Command Palette: ";
  ignore (Curses.nocbreak ());
  ignore (Curses.echo ());
  let s =
    if test_input_string = "" then read_string () else test_input_string
  in
  Dungeon.set_hud_text room
    (match s with
    | "help" ->
        String.concat "\n"
          (BatList.of_enum
             (if test_input_string = "" then BatFile.lines_of "data/help.txt"
              else BatFile.lines_of "../data/help.txt"))
    | "quit" ->
        Curses.endwin ();
        exit 0
    | _ -> s);
  ignore (Curses.noecho ());
  ignore (Curses.cbreak ())

let rec input_handling ?(cmd_palette_str = "") dungeon input =
  match input with
  | Keyboard.Up -> move_player dungeon Keyboard.Up
  | Keyboard.Down -> move_player dungeon Keyboard.Down
  | Keyboard.Right -> move_player dungeon Keyboard.Right
  | Keyboard.Left -> move_player dungeon Keyboard.Left
  | Keyboard.B -> Room.place_bomb (Dungeon.current_room dungeon)
  | Keyboard.Space -> Room.wait (Dungeon.current_room dungeon)
  | Keyboard.Enter ->
      if cmd_palette_str = "" then command_palette dungeon
      else command_palette ~test_input_string:cmd_palette_str dungeon
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()

let handle_input dungeon =
  let input = Input.read_input () in
  input_handling dungeon input

let damage_player () = failwith "TODO"

(* I just use [enemy_loc] and [debug_info] for debugging *)
(* [enemy_loc dungeon] gets the coordinates of the first enemy in the current room the player is in in the dungeon*)
let enemy_loc dungeon =
  try
    Coords.to_string
      (Enemies.get_position
         (List.hd (Room.get_enemies (Dungeon.current_room dungeon))))
  with Failure e when e = "hd" -> "no enemies"

let debug_info dungeon =
  "\nPlayer loc: "
  ^ Coords.to_string (Room.get_player_pos (Dungeon.current_room dungeon))
  ^ "\n1st Enemy Loc (in enemies list): " ^ enemy_loc dungeon
  ^ "\nPlayer Health: "
  ^ string_of_int (Player.health (Dungeon.player dungeon))
  ^ "\n1st Enemy dist from player: "
  ^
  try
    string_of_float
      (Coords.euclid_dist
         (Enemies.get_position
            (List.hd (Room.get_enemies (Dungeon.current_room dungeon))))
         (Room.get_player_pos (Dungeon.current_room dungeon)))
  with Failure e when e = "hd" -> "no enemies"

let hud_text dungeon =
  "Health: " ^ string_of_int (Player.health (Dungeon.player dungeon))

(* [process_world dungeon] now returns a bool indicating if the game is still
   going (i.e. the player's health > 0)*)
let process_world dungeon =
  (*main game loop*)
  handle_input dungeon;
  Room.explode (Dungeon.current_room dungeon);
  Room.update_enemies (Dungeon.current_room dungeon) (Dungeon.player dungeon);
  Dungeon.set_hud_text dungeon (hud_text dungeon);
  if Player.is_alive (Dungeon.player dungeon) then true else false

let test_input_handling ?(cmd_palette_str = "") =
  input_handling ~cmd_palette_str
