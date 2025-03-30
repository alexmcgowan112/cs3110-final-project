open OUnit2
open Cs3110_final_project

module TestInput = Keyboard.MakeInput (struct
  (** [arr] is an array of integers representing a sequence of key codes. *)
  let arr =
    [|
      98; 113; Curses.Key.up; Curses.Key.down; Curses.Key.left; Curses.Key.right;
    |]

  (** [ind] is a reference to an integer that tracks the current index in the
      [arr] array. *)
  let ind = ref 0

  (** [get ()] retrieves the current key code from the [arr] array based on the
      index stored in [ind]. If the index is within bounds, it increments [ind]
      and returns the key code at the current index. If the index is out of
      bounds, it returns [1]. *)
  let get () =
    let i = !ind in
    if i < Array.length arr then (
      let v = arr.(i) in
      ind := i + 1;
      v)
    else 1
end)

(** [string_of_list elem_to_string lst] converts a list [lst] into its string
    representation. Each element of the list is converted to a string using the
    function [elem_to_string], and the elements are separated by semicolons
    within square brackets.

    Example:
    - [string_of_list string_of_int [1; 2; 3]] evaluates to "[1;2;3;]".

    @param elem_to_string
      A function that converts an element of the list to a string.
    @param lst The list to be converted to a string.
    @return A string representation of the list. *)
let string_of_list elem_to_string lst =
  "[" ^ List.fold_left (fun acc el -> elem_to_string el ^ ";" ^ acc) "]" lst

(** [input_responses ()] reads a sequence of keyboard inputs until a the inputs
    return Keyboard.None (indicating no further input). It uses a reference to
    track the current input and collects the inputs in a list.

    @return
      A list of keyboard inputs read from the array in the above input module.
*)
let input_responses () =
  let curr_input = TestInput.read_input () in
  let out = [] in
  let rec helper curr lst =
    if curr <> Keyboard.None then curr :: helper (TestInput.read_input ()) lst
    else lst
  in
  helper curr_input out

let test_room () = Room.load_room_from_file "../data/rooms/simple.json"
let default_room = test_room ()

let rec run_inputs input_list =
  match input_list with
  | [] -> default_room
  | h :: t ->
      Game.test_input_handling default_room h;
      run_inputs t

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
             { x = 5; y = 4 } );
         ( "moving down moves player down" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Down ]))
             { x = 5; y = 6 } );
         ( "moving left moves player left" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Left ]))
             { x = 4; y = 5 } );
         ( "moving right moves player right" >:: fun _ ->
           assert_equal
             (Room.get_player_pos (run_inputs [ Keyboard.Right ]))
             { x = 6; y = 5 } );
         ( "running into a wall stops the player" >:: fun _ ->
           assert_equal
             (Room.get_player_pos
                (run_inputs [ Keyboard.Right; Keyboard.Right; Keyboard.Right ]))
             { x = 7; y = 5 } );
       ]

let _ = run_test_tt_main tests
