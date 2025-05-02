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
      let assert_coords_not_equal c1 c2 =
        Coords.equal c1 c2 |> not |> assert_bool "Equal"
      in
      assert_coords_not_equal { x = 5; y = 5 } { x = 6; y = 5 };
      assert_coords_not_equal { x = 5; y = 5 } { x = 5; y = 6 } );
    ( "compare function works" >:: fun _ ->
      assert_bool "x greater"
        (Coords.compare { x = 5; y = 5 } { x = 0; y = 5 } > 0);
      assert_bool "y greater"
        (Coords.compare { x = 5; y = 5 } { x = 5; y = 0 } > 0);
      assert_bool "x less"
        (Coords.compare { x = 5; y = 5 } { x = 10; y = 5 } < 0);
      assert_bool "y less"
        (Coords.compare { x = 5; y = 5 } { x = 5; y = 10 } < 0);
      assert_bool "both differ"
        (Coords.compare { x = 5; y = 5 } { x = 0; y = 0 } > 0);
      assert_equal 0
        (Coords.compare { x = 5; y = 5 } { x = 5; y = 5 })
        ~printer:string_of_int );
    ( "hash function obeys equality" >:: fun _ ->
      assert_bool "hash function gives the same value for equal coords"
        (Coords.hash { x = 5; y = 5 } = Coords.hash { x = 5; y = 5 });
      assert_bool "hash function gives a diffent value for non-equal coords"
        (Coords.hash { x = 0; y = 0 } <> Coords.hash { x = 5; y = 5 }) );
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
        (Coords.manhattan_dist { x = 2; y = 3 } { x = 6; y = 6 })
        7 ~cmp:Int.equal ~printer:string_of_int );
    ( "Chebyshev distance between two coordinates works" >:: fun _ ->
      let assert_chebyshev c1 c2 expected =
        assert_equal
          (Coords.chebyshev_dist c1 c2)
          expected ~cmp:Int.equal ~printer:string_of_int
      in
      assert_chebyshev { x = 2; y = 4 } { x = 6; y = 6 } 4;
      assert_chebyshev { x = 6; y = 5 } { x = 6; y = 6 } 1 );
    make_add_dir_test ({ x = 5; y = 5 }, 5, Keyboard.Up) { x = 5; y = 0 };
    make_add_dir_test ({ x = 5; y = 5 }, 3, Keyboard.Down) { x = 5; y = 8 };
    make_add_dir_test ({ x = 5; y = 5 }, 1, Keyboard.Left) { x = 4; y = 5 };
    make_add_dir_test ({ x = 5; y = 5 }, 2, Keyboard.Right) { x = 7; y = 5 };
    make_add_dir_test ({ x = 5; y = 5 }, 0, Keyboard.Up) { x = 5; y = 5 };
    ( "Invalid direction should raise a failure" >:: fun _ ->
      assert_raises (Failure "add_dir expects a direction") (fun () ->
          Coords.add_dir { x = 5; y = 5 } 1 Keyboard.None) );
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
        ~printer:(string_of_list Keyboard.to_string);
      assert_equal "None" (Keyboard.to_string Keyboard.None) ~cmp:String.equal
    );
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
          [| "v"; "."; "."; "."; "."; "O"; "."; "."; "."; "."; "$" |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "#"; "#"; "#"; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "#"; "."; "."; "."; "."; "."; "#"; "."; "." |];
          [| "."; "."; "#"; "."; "."; "@"; "."; "."; "#"; "."; "." |];
          [| "."; "."; "#"; "."; "."; "."; "."; "."; "#"; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "#"; "#"; "#"; "."; "."; "."; "." |];
          [| "."; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
          [| "~"; "."; "."; "."; "."; "."; "."; "."; "."; "."; "." |];
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

let make_graph () =
  let g = Connections.G.create () in
  (* Add a 11x11 grid of vertices and connect them as a grid *)
  for x = 0 to 10 do
    for y = 0 to 10 do
      let v = { Coords.x; y } in
      Connections.G.add_vertex g v
    done
  done;
  for x = 0 to 10 do
    for y = 0 to 10 do
      let v = { Coords.x; y } in
      let neighbors = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ] in
      List.iter
        (fun (nx, ny) ->
          if nx >= 0 && nx <= 10 && ny >= 0 && ny <= 10 then
            Connections.G.add_edge g v { Coords.x = nx; y = ny })
        neighbors
    done
  done;
  g

let explosion_tests =
  let graph = make_graph () in
  [
    ( "the center tile of an explosion explodes immediately" >:: fun _ ->
      assert_bool "Not exploding"
        (Explosion.tile_is_exploding { x = 5; y = 5 }
           [ Explosion.create { x = 5; y = 5 } 3 ]
           graph) );
    ( "a tile adjacent to an explosion explodes after 1 turn" >:: fun _ ->
      let exp = Explosion.create { x = 5; y = 5 } 3 in
      Explosion.spread exp;
      assert_bool "Not exploding"
        (Explosion.tile_is_exploding { x = 5; y = 6 } [ exp ] graph) );
    ( "a tile further from an explosion than its current radius does not explode"
    >:: fun _ ->
      let exp = Explosion.create { x = 5; y = 5 } 3 in
      Explosion.spread exp;
      assert_bool "Exploding"
        (not (Explosion.tile_is_exploding { x = 7; y = 7 } [ exp ] graph)) );
    ( "a newly created explosion is in progress" >:: fun _ ->
      let exp = Explosion.create { x = 5; y = 5 } 3 in
      assert_bool "Not in progress" (Explosion.is_in_progress exp) );
    ( "an explosion that has spread to its max radius is not in progress"
    >:: fun _ ->
      let exp = Explosion.create { x = 5; y = 5 } 0 in
      Explosion.spread exp;
      assert_bool "In progress" (not (Explosion.is_in_progress exp)) );
  ]

let hud_tests =
  [
    ( "command palette displays the help menu when asked" >:: fun _ ->
      let dungeon = Dungeon.create_test () in
      assert_equal
        (String.concat "\n"
           (BatList.of_enum (BatFile.lines_of "../data/help.txt")))
        (Option.get
           (Game.test_input_handling ~cmd_palette_str:"help" dungeon
              Keyboard.Enter))
        ~printer:Fun.id;
      assert_equal "Command Palette:" (Dungeon.hud_text dungeon) ~printer:Fun.id
        ~cmp:(fun s1 s2 -> String.trim s1 = String.trim s2) );
  ]

let enemy_tests =
  let graph = make_graph () in
  [
    ( "Default enemy has expected stats" >:: fun _ ->
      let enemy = Enemies.create { x = 5; y = 5 } "Ghost" in
      assert_equal
        { Coords.x = 5; Coords.y = 5 }
        (Enemies.get_position enemy)
        ~cmp:Coords.equal ~printer:Coords.to_string;
      Enemies.move enemy { x = 0; y = 0 };
      assert_equal
        { Coords.x = 0; Coords.y = 0 }
        (Enemies.get_position enemy)
        ~cmp:Coords.equal ~printer:Coords.to_string;
      assert_bool "Enemies start out alive" (Enemies.is_alive enemy);
      ignore (Enemies.take_damage enemy 1 (fun _ -> ()));
      assert_bool "Enemies don't instantly die from taking damage"
        (Enemies.is_alive enemy);
      ignore (Enemies.take_damage enemy 10000 (fun _ -> ()));
      assert_bool "Enemies die after taking too much damage"
        (not (Enemies.is_alive enemy)) );
    ( "Zombie moves toward player and attacks in range" >:: fun _ ->
      let enemy = Enemies.create { x = 5; y = 5 } "Zombie" in
      let player = Player.create () in
      let all_enemies = Array.make 1 (Some enemy) in
      let moved_enemy =
        Enemies.move_or_attack enemy { x = 5; y = 6 } player graph all_enemies
          (fun _ -> ())
      in
      let pos = Enemies.get_position moved_enemy in
      let dist = Coords.manhattan_dist pos { x = 5; y = 6 } in
      assert_bool "Zombie should be in range or have moved closer" (dist <= 1)
    );
    ( "Bomber explodes when in range of player" >:: fun _ ->
      let enemy = Enemies.create { x = 5; y = 5 } "Bomber" in
      let player = Player.create () in
      let all_enemies = Array.make 1 (Some enemy) in
      let explosion_created = ref false in
      let add_explosion exp =
        explosion_created := true;
        assert_equal { x = 5; y = 5 }
          (Explosion.get_position exp)
          ~cmp:Coords.equal ~printer:Coords.to_string
      in
      let _ =
        Enemies.move_or_attack enemy { x = 5; y = 6 } player graph all_enemies
          add_explosion
      in
      assert_bool "Bomber should have exploded" !explosion_created );
    ( "enemies move properly" >:: fun _ ->
      let dungeon =
        Dungeon.load_dungeon_from_file "../data/dungeons/test_enemies.json"
      in
      let room = Dungeon.current_room dungeon in
      for _ = 1 to 20 do
        Room.update_enemies room (Dungeon.player dungeon)
      done );
    ( "Bomber moves toward player when not in range" >:: fun _ ->
      let enemy = Enemies.create { x = 2; y = 2 } "Bomber" in
      let player = Player.create () in
      let all_enemies = Array.make 1 (Some enemy) in
      let explosion_created = ref false in
      let add_explosion _ = explosion_created := true in
      let moved_enemy =
        Enemies.move_or_attack enemy { x = 5; y = 5 } player (make_graph ())
          all_enemies add_explosion
      in
      let pos = Enemies.get_position moved_enemy in
      assert_bool "Bomber should not have exploded" (not !explosion_created);
      assert_bool "Bomber should have moved" (pos <> { x = 2; y = 2 });
      let old_dist = Coords.manhattan_dist { x = 2; y = 2 } { x = 5; y = 5 } in
      let new_dist = Coords.manhattan_dist pos { x = 5; y = 5 } in
      assert_bool "Bomber should be closer to player" (new_dist < old_dist) );
  ]

let game_tests =
  let dungeon = Dungeon.create_test () in
  [
    ( "The game starts out as expected" >:: fun _ ->
      assert_bool "The player should not start out dead"
        (Game.process_world dungeon) );
  ]

let item_tests =
  [
    ( "test picking up items" >:: fun _ ->
      let item =
        Item.create_item 0 Item (Some { Coords.x = 0; Coords.y = 0 })
      in
      assert_equal 0 (Item.get_item_id item) ~printer:string_of_int;
      assert_equal
        (Some { Coords.x = 0; Coords.y = 0 })
        (Item.get_location item)
        ~printer:(fun x -> Coords.to_string (Option.get x))
        ~cmp:(fun x y -> Coords.equal (Option.get x) (Option.get y));
      Item.pickup_item item;
      assert_equal None (Item.get_location item) );
    ( "test armor" >:: fun _ ->
      let player = Player.create () in
      assert_equal 0 (Player.total_armor player) ~printer:string_of_int;

      let armor_item1 = Item.create_item 1 (Armor { def = ref 10 }) None in
      assert_equal (Some 10) (Item.get_defense armor_item1) ~printer:(fun i ->
          Option.get i |> string_of_int);
      Player.equip player armor_item1;
      assert_equal 10 (Player.total_armor player) ~printer:string_of_int;

      let armor_item2 = Item.create_item 2 (Armor { def = ref 5 }) None in
      assert_equal (Some 5) (Item.get_defense armor_item2) ~printer:(fun i ->
          Option.get i |> string_of_int);
      Player.equip player armor_item2;
      assert_equal 15 (Player.total_armor player) ~printer:string_of_int;

      let non_armor_item = Item.create_item 3 Item None in
      assert_equal None (Item.get_defense non_armor_item);
      Player.equip player non_armor_item;
      assert_equal 15 (Player.total_armor player) ~printer:string_of_int;

      Player.damage player 4;
      assert_equal 11 (Player.total_armor player) ~printer:string_of_int;
      assert_equal 5 (Player.health player) ~printer:string_of_int;

      Player.damage player 12;
      assert_equal 0 (Player.total_armor player) ~printer:string_of_int;
      assert_equal 4 (Player.health player) ~printer:string_of_int );
    ( "test item to_string" >:: fun _ ->
      assert_equal "+" (Item.create_item 0 Item None |> Item.to_string);
      assert_equal "v"
        (Item.create_item 0 (Armor { def = ref 5 }) None |> Item.to_string);
      assert_equal "$" (Item.create_item 0 BiggerRadius None |> Item.to_string);
      assert_equal "~" (Item.create_item 0 ShorterFuse None |> Item.to_string)
    );
    ( "test other upgrade items" >:: fun _ ->
      let player = Player.create () in

      assert_equal 5 (Player.bombs player) ~printer:string_of_int;
      Player.add_bombs player 3;
      assert_equal 8 (Player.bombs player) ~printer:string_of_int;
      Player.remove_bombs player 4;
      assert_equal 4 (Player.bombs player) ~printer:string_of_int;
      Player.remove_bombs player 10;
      assert_equal 0 (Player.bombs player) ~printer:string_of_int;

      assert_equal 6 (Player.fuse_time player) ~printer:string_of_int;
      let shorter_fuse_item = Item.create_item 1 ShorterFuse None in
      Player.equip player shorter_fuse_item;
      assert_equal 5 (Player.fuse_time player) ~printer:string_of_int;

      assert_equal 1 (Player.blast_radius player) ~printer:string_of_int;
      let blast_radius_item = Item.create_item 2 BiggerRadius None in
      Player.equip player blast_radius_item;
      assert_equal 2 (Player.blast_radius player) ~printer:string_of_int;

      Player.equip player shorter_fuse_item;
      assert_equal 4 (Player.fuse_time player) ~printer:string_of_int );
    ( "equipping Health item sets player health to 5" >:: fun _ ->
      let player = Player.create () in
      Player.damage player 3;
      assert_equal 2 (Player.health player) ~printer:string_of_int;
      let health_item = Item.create_item 0 Health None in
      Player.equip player health_item;
      assert_equal 5 (Player.health player) ~printer:string_of_int );
    ( "equipping Bomb item increases player's bomb count" >:: fun _ ->
      let player = Player.create () in
      assert_equal 5 (Player.bombs player) ~printer:string_of_int;
      let bomb_item = Item.create_item 1 Bomb None in
      Player.equip player bomb_item;
      assert_equal 6 (Player.bombs player) ~printer:string_of_int );
  ]

let tests =
  "test suite"
  >::: coords_tests @ keyboard_tests @ room_tests @ explosion_tests @ hud_tests
       @ enemy_tests @ game_tests @ item_tests

let _ = run_test_tt_main tests
