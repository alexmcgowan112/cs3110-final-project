open Cs3110_final_project

module Input = Keyboard.MakeInput (struct
  let get = Curses.getch
end)

let new_colors () =
  ignore (Curses.start_color ());
  (* Define color pairs *)
  ignore (Curses.init_pair 1 Curses.Color.red Curses.Color.black);
  (*curses colors intiialized here because this function is called in the
    beginning. *)
  ignore (Curses.init_pair 2 Curses.Color.green Curses.Color.black);
  ignore (Curses.init_pair 3 Curses.Color.blue Curses.Color.black);
  ignore (Curses.init_pair 4 Curses.Color.yellow Curses.Color.black);
  ignore (Curses.init_pair 5 Curses.Color.cyan Curses.Color.black);
  ignore (Curses.init_pair 6 Curses.Color.magenta Curses.Color.black)

let window =
  let w = Curses.initscr () in
  Curses.scrollok w false;
  ignore (Curses.start_color ());
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  ignore (Curses.keypad w true);
  ignore (Curses.curs_set 0);
  ignore (Curses.mousemask 0);
  (* Enable color mode *)
  if Curses.has_colors () then new_colors ();
  w

(*let print_from_file path = BatEnum.iter print_endline (BatFile.lines_of
  path)*)
let room =
  if Array.length Sys.argv = 1 then Room.new_room ()
  else
    match Sys.argv.(1) with
    | "maze" -> Room.load_room_from_file "data/rooms/maze.json"
    | "empty" -> Room.load_room_from_file "data/rooms/empty.json"
    | _ -> Room.new_room ()

let previous_direction = ref Keyboard.ArrowUp

let move_player dir =
  Room.move_player room dir;
  previous_direction := dir

(*TODO: temporary function. modify when changing implementation in future*)
let match_characters win i j = function
  | "*" ->
      ignore (Curses.attron (Curses.A.color_pair 1));
      (*If you need to do stuff, change this # ^*)
      ignore (Curses.mvwaddstr win i j "*");
      ignore (Curses.attroff (Curses.A.color_pair 1))
  | x ->
      ignore (Curses.attron (Curses.A.color_pair 0));
      (*0 is default, the rest match the indexes up top. ctrl+F for init_pair*)
      ignore (Curses.mvwaddstr win i j x);
      ignore (Curses.attroff (Curses.A.color_pair 0))

let print_string_array win (arr : string array array) =
  let rows = Array.length arr in
  let cols = if rows > 0 then Array.length arr.(0) else 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      match_characters win i (j * 2) arr.(i).(j)
    done
  done

let print_room () =
  Curses.erase ();
  Curses.clearok window false;
  (* ignore (Curses.mvwaddstr window 0 0 (Room.to_string room)); *)
  ignore (print_string_array window (Room.to_string_array room));
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
    Unix.sleepf 0.3
  done

let rec game_loop () =
  print_room ();
  let input = Input.read_input () in
  (match input with
  | Keyboard.ArrowUp -> move_player Keyboard.ArrowUp
  | Keyboard.ArrowDown -> move_player Keyboard.ArrowDown
  | Keyboard.ArrowRight -> move_player Keyboard.ArrowRight
  | Keyboard.ArrowLeft -> move_player Keyboard.ArrowLeft
  | Keyboard.B -> Room.place_bomb room
  | Keyboard.E -> handle_explosion ()
  | Keyboard.Q ->
      Curses.endwin ();
      exit 0
  | Keyboard.None -> ());
  if Room.exploding room then
    while Room.exploding room do
      Room.explode room;
      print_room ();
      flush stdout;
      Unix.sleepf 0.3
    done;
  game_loop ()

let () = game_loop ()
