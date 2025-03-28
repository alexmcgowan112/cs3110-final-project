module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let move_player room dir = Room.move_player room dir

let rec input_handling room =
  let input = Input.read_input () in
  match input with
  | Keyboard.ArrowUp | Keyboard.W -> Room.move_player room Keyboard.ArrowUp
  | Keyboard.ArrowDown | Keyboard.S -> Room.move_player room Keyboard.ArrowDown
  | Keyboard.ArrowRight | Keyboard.D ->
      Room.move_player room Keyboard.ArrowRight
  | Keyboard.ArrowLeft | Keyboard.A -> Room.move_player room Keyboard.ArrowLeft
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ()
