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

let dungeon = Dungeon.load_dungeon_from_file "data/dungeons/medium.json"

(*TODO: temporary function. modify when changing implementation in future*)
let match_characters i j = function
  | "*" ->
      ignore (Curses.attron (Curses.A.color_pair 1));
      (*If you need to do stuff, change this # ^*)
      ignore (Curses.mvaddstr i j "*");
      ignore (Curses.attroff (Curses.A.color_pair 1))
  | x ->
      ignore (Curses.attron (Curses.A.color_pair 0));
      (*0 is default, the rest match the indexes up top. ctrl+F for init_pair*)
      ignore (Curses.mvaddstr i j x);
      ignore (Curses.attroff (Curses.A.color_pair 0))

let print_string_colors string_matrix =
  Array.iteri
    (fun i row -> Array.iteri (fun j str -> match_characters i (j * 2) str) row)
    string_matrix

let print_current_room () =
  Curses.erase ();
  Curses.clearok window false;
  (* ignore (Curses.mvwaddstr window 0 0 (Room.to_string room)); *)
  ignore
    (print_string_colors (Room.to_string_matrix (Dungeon.current_room dungeon)));
  ignore (Curses.addstr ("\n" ^ Dungeon.hud_text dungeon));
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

let print_game_over () =
  ignore (Curses.endwin ());
  print_endline "\n------------\n\rYou Died :(\n\r------------\n\r"

let rec game_loop () =
  print_current_room ();
  Curses.flushinp ();
  let game_continue = Game.process_world dungeon in
  if game_continue then game_loop () else print_game_over ()

let main_menu () =
  let get_menu_from_file path =
    "\n"
    ^ (BatFile.lines_of path |> BatList.of_enum
      |> List.map (fun s -> "  " ^ s)
      |> String.concat "\n")
    ^ "\n\n\n\
      \  (Press space to start or any other key to go back to the main menu!)"
  in

  let start, help =
    (get_menu_from_file "data/start.txt", get_menu_from_file "data/help.txt")
  in

  let on_start = ref true in

  let rec main_menu_loop () =
    Curses.erase ();
    Curses.clearok window false;
    ignore (Curses.addstr (if !on_start then start else help));
    ignore (Curses.refresh ());
    match Curses.getch () with
    | 113 (* q *) ->
        Curses.endwin ();
        exit 0
    | 32 (* space *) -> game_loop ()
    | 409 (* scroll *) -> main_menu_loop ()
    | _ ->
        on_start := not !on_start;
        main_menu_loop ()
  in

  main_menu_loop ()

let () = main_menu ()
