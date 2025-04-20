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

let command_palette room =
  Dungeon.set_hud_text room "Command Palette: ";
  ignore (Curses.nocbreak ());
  ignore (Curses.echo ());
  let s = read_string () in
  Dungeon.set_hud_text room
    (match s with
    | "help" ->
        String.concat "\n" (BatList.of_enum (BatFile.lines_of "data/help.txt"))
    | "quit" ->
        Curses.endwin ();
        exit 0
    | _ -> s);
  ignore (Curses.noecho ());
  ignore (Curses.cbreak ())

let rec input_handling dungeon input =
  match input with
  | Keyboard.Up -> move_player dungeon Keyboard.Up
  | Keyboard.Down -> move_player dungeon Keyboard.Down
  | Keyboard.Right -> move_player dungeon Keyboard.Right
  | Keyboard.Left -> move_player dungeon Keyboard.Left
  | Keyboard.B -> Room.place_bomb (Dungeon.current_room dungeon)
  | Keyboard.Space -> Room.wait (Dungeon.current_room dungeon)
  | Keyboard.Enter -> command_palette dungeon
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()

let handle_input dungeon =
  let input = Input.read_input () in
  input_handling dungeon input

let update_enemies () = ()
let damage_player () = failwith "TODO"

let process_world dungeon =
  (*main game loop*)
  handle_input dungeon;
  Room.explode (Dungeon.current_room dungeon);
  update_enemies ();
  Dungeon.set_hud_text dungeon
    (Coords.to_string (Room.get_player_pos (Dungeon.current_room dungeon)))

let test_input_handling = input_handling
