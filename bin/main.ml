open Cs3110_final_project
open Keyboard

module Input = Keyboard (struct
  let get = Curses.getch
end)

let window =
  let w = Curses.initscr () in
  Curses.scrollok w false;
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  ignore (Curses.keypad w true);
  ignore (Curses.curs_set 0);
  w

(*let print_from_file path = BatEnum.iter print_endline (BatFile.lines_of
  path)*)
let room = Room.new_room ()
let previous_direction = ref Room.Up

let move_player dir =
  Room.move_player room dir;
  previous_direction := dir

let print_room () =
  Curses.erase ();
  Curses.clearok window false;
  ignore (Curses.mvwaddstr window 0 0 (Room.to_string room));
  ignore (Curses.refresh ())

let handle_explosion () =
  print_string "Enter the x-coord of the explosion center (top left is 0,0): ";
  let x_coord = read_int () in
  let () =
    print_string "Enter the y-coord of the explosion center (top left is 0,0): "
  in
  let y_coord = read_int () in
  let () =
    print_string "Enter the maximum radius of the explosion (in lines): "
  in
  let radius = read_int () in
  Room.start_exploding room x_coord y_coord radius;
  while Room.exploding room do
    Room.explode room;
    print_room ();
    flush stdout;
    Unix.sleepf 0.3
  done

let rec game_loop () =
  print_room ();
  let input = Input.read_input () in
  (match input with
  | Input.ArrowUp -> move_player Room.Up
  | Input.ArrowDown -> move_player Room.Down
  | Input.ArrowRight -> move_player Room.Right
  | Input.ArrowLeft -> move_player Room.Left
  | Input.B -> Room.place_bomb room
  | Input.E -> handle_explosion ()
  | Input.Q ->
      Curses.endwin ();
      exit 0
  | Input.None -> ());
  if Room.exploding room then
    while Room.exploding room do
      Room.explode room;
      print_room ();
      flush stdout;
      Unix.sleepf 0.3
    done;
  game_loop ()

let () = game_loop ()
