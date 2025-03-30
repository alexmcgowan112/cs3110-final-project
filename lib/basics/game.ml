module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player room dir = Room.move_player room dir

let rec input_handling room input =
  match input with
  | Keyboard.Up -> move_player room Keyboard.Up
  | Keyboard.Down -> move_player room Keyboard.Down
  | Keyboard.Right -> move_player room Keyboard.Right
  | Keyboard.Left -> move_player room Keyboard.Left
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.Space -> Room.wait room
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()

let handle_input room =
  let input = Input.read_input () in
  input_handling room input

let test_input_handling = input_handling
