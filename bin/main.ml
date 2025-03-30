open Cs3110_final_project

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
  ignore (Curses.mousemask 1);
  (* Enable color mode *)
  if Curses.has_colors () then new_colors ();
  w

(*let print_from_file path = BatEnum.iter print_endline (BatFile.lines_of
  path)*)

(** [room] is a new room by default, or the maze/empty room if those args are
    given.*)
let room =
  if Array.length Sys.argv = 1 then Room.new_room ()
  else
    match Sys.argv.(1) with
    | "maze" -> Room.load_room_from_file "data/rooms/maze.json"
    | "empty" -> Room.load_room_from_file "data/rooms/empty.json"
    | _ -> Room.new_room ()

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
  Array.iteri
    (fun i row ->
      Array.iteri (fun j str -> match_characters win i (j * 2) str) row)
    arr

let print_room () =
  Curses.erase ();
  Curses.clearok window false;
  (* ignore (Curses.mvwaddstr window 0 0 (Room.to_string room)); *)
  ignore (print_string_array window (Room.to_string_matrix room));
  ignore (Curses.refresh ())

let read_int_input prompt y x =
  ignore (Curses.move y x);
  Curses.clrtoeol ();
  ignore (Curses.addstr prompt);
  ignore (Curses.echo ());
  ignore (Curses.refresh ());

  let buf = Buffer.create 10 in
  let rec read_input () =
    let ch = Curses.getch () in
    if ch = Curses.Key.enter || ch = int_of_char '\n' || ch = int_of_char '\r'
    then
      if Buffer.length buf > 0 then int_of_string (Buffer.contents buf)
      else read_input ()
    else if ch >= int_of_char '0' && ch <= int_of_char '9' then (
      Buffer.add_char buf (char_of_int ch);
      read_input ())
    else read_input ()
  in
  let result = read_input () in
  ignore (Curses.noecho ());
  result

let rec game_loop () =
  print_room ();
  Game.handle_input room;
  while Room.exploding room do
    Room.explode room;
    print_room ();
    Unix.sleepf 0.1
  done;
  Curses.flushinp ();
  game_loop ()

let () = game_loop ()
