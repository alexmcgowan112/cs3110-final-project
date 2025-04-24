open OUnit2
open Cs3110_final_project
open Testing

let coords_tests =
  let make_add_dir_test (coord, n, dir) expected =
    "taking " ^ string_of_int n ^ " step in the direction "
    ^ Keyboard.to_string dir ^ " from " ^ Coords.to_string coord
    ^ " should result in " ^ Coords.to_string expected
    >:: fun _ ->
    assert_equal expected
      (Coords.add_dir coord n dir)
      ~cmp:Coords.equal ~printer:Coords.to_string
  in

  [
    ( "coordinates equal in both elements are equal" >:: fun _ ->
      assert_bool "Not equal" (Coords.equal { x = 5; y = 5 } { x = 5; y = 5 })
    );
    ( "coordinates not equal in either element are not equal" >:: fun _ ->
      assert_bool "Equal" (not (Coords.equal { x = 5; y = 5 } { x = 6; y = 5 }));
      assert_bool "Equal" (not (Coords.equal { x = 5; y = 5 } { x = 5; y = 6 }))
    );
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
    make_add_dir_test ({ x = 5; y = 5 }, 5, Keyboard.Up) { x = 5; y = 0 };
    make_add_dir_test ({ x = 5; y = 5 }, 3, Keyboard.Down) { x = 5; y = 8 };
    make_add_dir_test ({ x = 5; y = 5 }, 1, Keyboard.Left) { x = 4; y = 5 };
    make_add_dir_test ({ x = 5; y = 5 }, 2, Keyboard.Right) { x = 7; y = 5 };
    make_add_dir_test ({ x = 5; y = 5 }, 0, Keyboard.Up) { x = 5; y = 5 };
    make_add_dir_test ({ x = 5; y = 5 }, 5, Keyboard.None) { x = 5; y = 5 };
  ]

let keyboard_tests =
  [
    ( "keyboard input test all keys" >:: fun _ ->
      assert_equal
        [
          Keyboard.B;
          Keyboard.Q;
          Keyboard.Up;
          Keyboard.Left;
          Keyboard.Down;
          Keyboard.Right;
          Keyboard.Space;
          Keyboard.Enter;
          Keyboard.Up;
          Keyboard.Down;
          Keyboard.Left;
          Keyboard.Right;
        ]
        (input_responses ())
        ~printer:(string_of_list Keyboard.to_string) );
  ]

let room_tests =
  let make_movement_test direction expected =
    let dir_str = Keyboard.to_string direction |> String.lowercase_ascii in
    "moving " ^ dir_str ^ " moves player " ^ dir_str >:: fun _ ->
    assert_equal expected
      (Room.get_player_pos (run_inputs [ direction ]))
      ~cmp:Coords.equal ~printer:Coords.to_string
  in

  [
    ( "getting player position at the start is equal to the starting position"
    >:: fun _ ->
      assert_equal
        (Room.get_player_pos (run_inputs []))
        { x = 5; y = 5 } ~cmp:Coords.equal ~printer:Coords.to_string );
    make_movement_test Keyboard.Up { x = 5; y = 4 };
    make_movement_test Keyboard.Down { x = 5; y = 6 };
    make_movement_test Keyboard.Left { x = 4; y = 5 };
    make_movement_test Keyboard.Right { x = 6; y = 5 };
    ( "an empty room is represented by the correct string" >:: fun _ ->
      assert_equal
        [|
          [| "."; "."; "."; "."; "."; "O"; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "#"; "#"; "#"; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "#"; "."; "."; "."; "."; "."; "#"; "."; "." |];
          [| "."; "."; "#"; "."; "."; "@"; "."; "."; "#"; "."; "." |];
          [| "."; "."; "#"; "."; "."; "."; "."; "."; "#"; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "#"; "#"; "#"; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
        |]
        (Room.to_string_matrix
           (Room.load_room_from_file "../data/rooms/test_rooms/simple_test.json"))
        ~printer:string_of_matrix );
    ( "running into a wall stops the player" >:: fun _ ->
      assert_equal
        (Room.get_player_pos
           (run_inputs [ Keyboard.Right; Keyboard.Right; Keyboard.Right ]))
        (Room.get_player_pos (run_inputs [ Keyboard.Right; Keyboard.Right ]))
        ~cmp:Coords.equal ~printer:Coords.to_string );
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
  ]

let explosion_tests =
  [
    ( "the center tile of an explosion explodes immediately" >:: fun _ ->
      assert_bool "Not exploding"
        (Explosion.tile_is_exploding { x = 5; y = 5 }
           (Explosion.create { x = 5; y = 5 } 3)) );
    ( "a tile adjacent to an explosion explodes after 1 turn" >:: fun _ ->
      let exp = Explosion.create { x = 5; y = 5 } 3 in
      Explosion.spread exp;
      assert_bool "Not exploding"
        (Explosion.tile_is_exploding { x = 5; y = 6 } exp) );
    ( "a tile further from an explosion than its current radius does not explode"
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
  ]

let tests =
  "test suite" >::: coords_tests @ keyboard_tests @ room_tests @ explosion_tests

let _ = run_test_tt_main tests
