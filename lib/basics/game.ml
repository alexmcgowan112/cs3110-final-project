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

(* UPDATE: this now returns a string that should be displayed for the command
   palette on the HUD. Its actually displayed by the main game loop function (in
   this file, below) *)
let command_palette ?(test_input_string = "") dungeon =
  Dungeon.set_hud_text dungeon "Command Palette: ";
  ignore (Curses.nocbreak ());
  ignore (Curses.echo ());
  let s =
    if test_input_string <> "" then test_input_string else read_string ()
  in
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  match s with
  | "help" ->
      String.concat "\n"
        (BatList.of_enum
           (if test_input_string = "" then BatFile.lines_of "data/help.txt"
            else BatFile.lines_of "../data/help.txt"))
  | "quit" ->
      Curses.endwin ();
      exit 0
  | _ -> "Unknown Command"

let rec input_handling ?(cmd_palette_str = "") dungeon input =
  match input with
  | Keyboard.Up | Keyboard.Down | Keyboard.Right | Keyboard.Left ->
      move_player dungeon input;
      None
  | Keyboard.B ->
      Room.place_bomb (Dungeon.current_room dungeon) (Dungeon.player dungeon);
      None
  | Keyboard.Space ->
      Room.wait (Dungeon.current_room dungeon) (Dungeon.player dungeon);
      None
  | Keyboard.Enter ->
      if cmd_palette_str <> "" then
        Some (command_palette ~test_input_string:cmd_palette_str dungeon)
      else Some (command_palette dungeon)
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> None

let handle_input dungeon =
  let input = Input.read_input () in
  input_handling dungeon input

let timestep = ref 0

let hud_text dungeon =
  "Health: "
  ^ string_of_int (Player.health (Dungeon.player dungeon))
  ^ " | Armor: "
  ^ string_of_int (Player.total_armor (Dungeon.player dungeon))
  ^ " | Bombs: "
  ^ string_of_int (Player.bombs (Dungeon.player dungeon))

(* [process_world dungeon] now returns a bool indicating if the game is still
   going (i.e. the player's health > 0)*)
let process_world dungeon =
  (*main game loop*)
  let cmd_palette_display = handle_input dungeon in
  Room.update_items (Dungeon.current_room dungeon) (Dungeon.player dungeon);
  Room.update_enemies (Dungeon.current_room dungeon) (Dungeon.player dungeon);
  Room.update_player_health (Dungeon.current_room dungeon) (Dungeon.player dungeon);
  Room.explode (Dungeon.current_room dungeon);
  Dungeon.set_hud_text dungeon
    (hud_text dungeon
    ^
    match cmd_palette_display with
    | Some msg -> "\n" ^ msg
    | None -> "\nPress Enter to open the command palette.");
  if Player.is_alive (Dungeon.player dungeon) then true else false

let test_input_handling ?(cmd_palette_str = "") =
  input_handling ~cmd_palette_str
