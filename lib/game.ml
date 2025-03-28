module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player room dir = Room.move_player room dir

let rec input_handling room =
  let input = Input.read_input () in
  match input with
  | Keyboard.ArrowUp -> move_player room Keyboard.ArrowUp
  | Keyboard.ArrowDown -> move_player room Keyboard.ArrowDown
  | Keyboard.ArrowRight -> move_player room Keyboard.ArrowRight
  | Keyboard.ArrowLeft -> move_player room Keyboard.ArrowLeft
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()
