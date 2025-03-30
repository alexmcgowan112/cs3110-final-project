open OUnit2
open Cs3110_final_project
open Testing

let tests =
  "test suite"
  >::: [
         ( "keyboard input test all keys" >:: fun _ ->
           assert_equal
             [
               Keyboard.B;
               Keyboard.Q;
               Keyboard.Up;
               Keyboard.Down;
               Keyboard.Left;
               Keyboard.Right;
             ]
             (input_responses ())
             ~printer:(string_of_list TestInput.string_of_input) );
         ( "moving up moves player up" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Up ]))
             { x = 5; y = 4 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "moving down moves player down" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Down ]))
             { x = 5; y = 6 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "moving left moves player left" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Left ]))
             { x = 4; y = 5 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "moving right moves player right" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Right ]))
             { x = 6; y = 5 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "running into a wall stops the player" >:: fun _ ->
           assert_equal
             (Room.get_player_pos
                (run_inputs [ Keyboard.Right; Keyboard.Right; Keyboard.Right ]))
             (Room.get_player_pos
                (run_inputs [ Keyboard.Right; Keyboard.Right ]))
             ~cmp:Coords.equal ~printer:Coords.to_string );
       ]

let _ = run_test_tt_main tests
