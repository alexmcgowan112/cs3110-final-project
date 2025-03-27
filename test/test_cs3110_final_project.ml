open OUnit2
open Cs3110_final_project

module Input = Keyboard.MakeInput (struct
  let arr =
    [|
      98;
      113;
      101;
      Curses.Key.up;
      Curses.Key.down;
      Curses.Key.left;
      Curses.Key.right;
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

let room = Room.new_room ()

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

let rec run_inputs input_list =
  match input_list with
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
      | Keyboard.None -> ()
      | Keyboard.E -> ());
      if Room.exploding room then
        while Room.exploding room do
          Room.explode room;
          Unix.sleepf 0.3
        done;
      run_inputs t

let tests =
  "test suite"
  >::: [
         ( "keyboard input test all keys" >:: fun _ ->
           assert_equal
             [
               Keyboard.B;
               Keyboard.Q;
               Keyboard.E;
               Keyboard.ArrowUp;
               Keyboard.ArrowDown;
               Keyboard.ArrowLeft;
               Keyboard.ArrowRight;
             ]
             (input_responses ())
             ~printer:(string_of_list Input.string_of_input) );
       ]

let _ = run_test_tt_main tests
