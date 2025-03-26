open Cs3110_final_project

let print_from_file path = BatEnum.iter print_endline (BatFile.lines_of path)
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
  erase_previous_lines 100;
  print_endline (Room.to_string room)

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
  print_string
    "Input a direction to move (up, down, left, or right), \"e\" for an \
     explosion, \"b\" to place a bomb with a fuse, or \"q\" to quit: ";
  let line = read_line () in
  if String.length line > 0 then
    match Char.lowercase_ascii (String.get line 0) with
    | 'u' -> move_player Room.Up
    | 'd' -> move_player Room.Down
    | 'l' -> move_player Room.Left
    | 'r' -> move_player Room.Right
    | 'b' -> Room.place_bomb room
    | 'e' -> handle_explosion ()
    | 'q' -> exit 0
    | _ -> ()
  else Room.move_player room !previous_direction;
  if Room.exploding room then
    while Room.exploding room do
      Room.explode room;
      print_room ();
      flush stdout;
      Unix.sleepf 0.3
    done;
  game_loop ()

let () =
  print_endline (Room.to_string room ^ "\n");
  game_loop ()
