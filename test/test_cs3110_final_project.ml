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
         ( "getting player position at the start is equal to the starting \
            position"
         >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs []))
             { x = 5; y = 5 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "running the bomb fuse out explodes it" >:: fun _ ->
           assert_bool "No explosion occuring"
             (Room.exploding
                (*Bomb fuse is 6, so we need to wait 6 turns before exploding*)
                (run_inputs
                   [
                     Keyboard.B;
                     Keyboard.Space;
                     Keyboard.Space;
                     Keyboard.Space;
                     Keyboard.Space;
                     Keyboard.Space;
                     Keyboard.Space;
                   ])) );
         ( "the center tile of an explosion explodes immediately" >:: fun _ ->
           assert_bool "Not exploding"
             (Explosion.tile_is_exploding { x = 5; y = 5 }
                (Explosion.create { x = 5; y = 5 } 3)) );
         ( "a tile adjacent to an explosion explodes after 1 turn" >:: fun _ ->
           let exp = Explosion.create { x = 5; y = 5 } 3 in
           Explosion.spread exp;
           assert_bool "Not exploding"
             (Explosion.tile_is_exploding { x = 5; y = 6 } exp) );
         ( "a tile further from an explosion than its current radius does not \
            explode"
         >:: fun _ ->
           let exp = Explosion.create { x = 5; y = 5 } 3 in
           Explosion.spread exp;
           assert_bool "Exploding"
             (not (Explosion.tile_is_exploding { x = 7; y = 7 } exp)) );
         ( "a newly created explosion is in progress" >:: fun _ ->
           let exp = Explosion.create { x = 5; y = 5 } 3 in
           assert_bool "Not in progress" (Explosion.is_in_progress exp) );
         ( "an explosion that has spread to its max radius is not in progress"
         >:: fun _ ->
           let exp = Explosion.create { x = 5; y = 5 } 0 in
           Explosion.spread exp;
           assert_bool "In progress" (not (Explosion.is_in_progress exp)) );
         ( "coordinates equal in both elements are equal" >:: fun _ ->
           assert_bool "Not equal"
             (Coords.equal { x = 5; y = 5 } { x = 5; y = 5 }) );
         ( "coordinates not equal in either element are not equal" >:: fun _ ->
           assert_bool "Equal"
             (not (Coords.equal { x = 5; y = 5 } { x = 6; y = 5 }));
           assert_bool "Equal"
             (not (Coords.equal { x = 5; y = 5 } { x = 5; y = 6 })) );
         ( "adding two coordinates is a coordinate with the elements summed"
         >:: fun _ ->
           assert_equal
             (Coords.add { x = 5; y = 5 } { x = 6; y = 6 })
             { x = 11; y = 11 } ~cmp:Coords.equal ~printer:Coords.to_string );
         ( "Euclidean distance between two coordinates works" >:: fun _ ->
           assert_equal
             ~cmp:(cmp_float ~epsilon:0.0001)
             (Coords.euclid_dist { x = 5; y = 5 } { x = 6; y = 6 })
             1.4142135623730951 ~printer:string_of_float );
         ( "Manhattan distance between two coordinates works" >:: fun _ ->
           assert_equal
             (Coords.manhattan_dist { x = 5; y = 5 } { x = 6; y = 6 })
             2 ~cmp:Int.equal ~printer:string_of_int );
       ]

let _ = run_test_tt_main tests
