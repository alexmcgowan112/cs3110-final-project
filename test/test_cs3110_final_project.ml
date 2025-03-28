open OUnit2
open Cs3110_final_project

module Input = Keyboard.MakeInput (struct
  let arr =
    [|
      98; 113; Curses.Key.up; Curses.Key.down; Curses.Key.left; Curses.Key.right;
    |]

  let ind = ref 0

  let get () =
    let i = !ind in
    if i < Array.length arr then (
      let v = arr.(i) in
      ind := i + 1;
      v)
    else 1
end)

let string_of_list elem_to_string lst =
  "[" ^ List.fold_left (fun acc el -> elem_to_string el ^ ";" ^ acc) "]" lst

let input_responses () =
  let curr_input = ref (Input.read_input ()) in
  let out = [] in
  let rec helper curr lst =
    if !curr_input <> Keyboard.None then (
      let v = !curr in
      curr := Input.read_input ();
      v :: helper curr lst)
    else lst
  in
  helper curr_input out

let test_room () = Room.load_room_from_file "../data/rooms/simple.json"
let default_room = test_room ()

let run_inputs input_list =
  let room = test_room () in
  let rec run_inputs_helper lst =
    match lst with
    | [] -> ()
    | input :: t ->
        (match input with
        | Keyboard.ArrowUp -> Room.move_player room Keyboard.ArrowUp
        | Keyboard.ArrowDown -> Room.move_player room Keyboard.ArrowDown
        | Keyboard.ArrowRight -> Room.move_player room Keyboard.ArrowRight
        | Keyboard.ArrowLeft -> Room.move_player room Keyboard.ArrowLeft
        | Keyboard.B -> Room.place_bomb room
        | Keyboard.Q ->
            Curses.endwin ();
            exit 0
        | Keyboard.None -> ());
        if Room.exploding room then
          while Room.exploding room do
            Room.explode room;
            Unix.sleepf 0.3
          done;
        run_inputs_helper t
  in
  run_inputs_helper input_list;
  room

let tests =
  "test suite"
  >::: [
         ( "keyboard input test all keys" >:: fun _ ->
           assert_equal
             [
               Keyboard.B;
               Keyboard.Q;
               Keyboard.ArrowUp;
               Keyboard.ArrowDown;
               Keyboard.ArrowLeft;
               Keyboard.ArrowRight;
             ]
             (input_responses ())
             ~printer:(string_of_list Input.string_of_input) );
         ( "moving up moves player up" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.ArrowUp ]))
             { x = 5; y = 4 } );
         ( "moving down moves player down" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.ArrowDown ]))
             { x = 5; y = 6 } );
         ( "moving left moves player left" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.ArrowLeft ]))
             { x = 4; y = 5 } );
         ( "moving right moves player right" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.ArrowRight ]))
             { x = 6; y = 5 } );
         ( "running into a wall stops the player" >:: fun _ ->
           assert_equal
             (Room.get_player_pos
                (run_inputs
                   [
                     Keyboard.ArrowRight;
                     Keyboard.ArrowRight;
                     Keyboard.ArrowRight;
                   ]))
             { x = 7; y = 5 } );
       ]

let _ = run_test_tt_main tests
