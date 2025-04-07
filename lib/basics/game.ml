module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player room dir = Room.move_player room dir

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
  Room.set_hud_text room "Command Palette: ";
  ignore (Curses.nocbreak ());
  ignore (Curses.echo ());
  let s = read_string () in
  Room.set_hud_text room
    (if s = "help" then
       String.concat "\n" (BatList.of_enum (BatFile.lines_of "data/help.txt"))
     else s);
  ignore (Curses.noecho ());
  ignore (Curses.cbreak ())
(*Can't just remove this getch, as that means the refresh in the main print_room
  function will wipe any output we give before it's really visible to the user,
  so we have to block till they give input.*)

let rec input_handling room input =
  match input with
  | Keyboard.Up -> move_player room Keyboard.Up
  | Keyboard.Down -> move_player room Keyboard.Down
  | Keyboard.Right -> move_player room Keyboard.Right
  | Keyboard.Left -> move_player room Keyboard.Left
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.Space -> Room.wait room
  | Keyboard.Enter -> command_palette room
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()

let handle_input room =
  let input = Input.read_input () in
  input_handling room input

let test_input_handling = input_handling
