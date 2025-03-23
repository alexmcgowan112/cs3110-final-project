open Cs3110_final_project

let room = Room.new_room ()
let previous_direction = ref Room.Up

let erase_previous_lines n =
  for _ = 1 to n do
    Printf.printf "\027[1A\027[2K" (* Move cursor up, clear line *)
  done;
  flush stdout

let move_player dir =
  Room.move_player room dir;
  previous_direction := dir

let print_room () =
  erase_previous_lines 12;
  print_endline (Room.to_string room)

let rec game_loop () =
  print_room ();
  print_string "Input a direction to move (up, down, left, or right): ";
  let line = read_line () in
  if String.length line > 0 then
    match Char.lowercase_ascii (String.get line 0) with
    | 'u' -> move_player Room.Up
    | 'd' -> move_player Room.Down
    | 'l' -> move_player Room.Left
    | 'r' -> move_player Room.Right
    | _ -> ()
  else Room.move_player room !previous_direction;
  game_loop ()

let () =
  print_endline (Room.to_string room ^ "\n");
  game_loop ()
