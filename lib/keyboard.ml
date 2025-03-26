open Curses

type t =
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | B
  | None

let ncurses_init () =
  let stdscr = initscr () in
  ignore (cbreak ());
  ignore (noecho ());
  ignore (keypad stdscr true);
  ()

let read_input () =
  ncurses_init ();
  let result =
    match getch () with
    | 98 -> B
    | code when code = Key.up -> ArrowUp
    | code when code = Key.down -> ArrowDown
    | code when code = Key.left -> ArrowLeft
    | code when code = Key.right -> ArrowRight
    | _ -> None
  in

  endwin ();
  result
