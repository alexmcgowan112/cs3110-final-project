open Curses

let () =
  (* Initialize ncurses *)
  let stdscr = initscr () in
  ignore (cbreak ());
  ignore (noecho ());
  ignore (keypad stdscr true);

  (* Enable color mode *)
  if has_colors () then (
    ignore (start_color ());
    (* Define color pairs *)
    ignore (init_pair 1 Color.red Color.black);
    ignore (init_pair 2 Color.green Color.black);
    ignore (init_pair 3 Color.blue Color.black);
    ignore (init_pair 4 Color.yellow Color.black);
    ignore (init_pair 5 Color.cyan Color.black);
    ignore (init_pair 6 Color.magenta Color.black));

  (* Extract arrow key sequences *)
  let arrow_keys =
    [
      (Key.up, "Up");
      (Key.down, "Down");
      (Key.left, "Left");
      (Key.right, "Right");
    ]
  in

  ignore (addstr "Press arrow keys (ESC to exit):\n");
  ignore (refresh ());

  let rec loop () =
    match getch () with
    | 27 -> () (* Escape key to exit *)
    | code ->
        let arrow =
          try Some (List.assoc code arrow_keys) with Not_found -> None
        in
        (match arrow with
        | Some dir -> ignore (addstr (Printf.sprintf "Arrow key: %s\n" dir))
        | None -> ignore (addstr (Printf.sprintf "Unrecognized key: %d\n" code)));
        ignore (refresh ());
        loop ()
  in

  loop ();

  (* End ncurses mode *)
  endwin ()
