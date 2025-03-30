module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player room dir = Room.move_player room dir

let input_handling room =
  let input = Input.read_input () in
  match input with
  | Keyboard.Up | Keyboard.Down | Keyboard.Right | Keyboard.Left ->
      Room.move_player room input
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()
