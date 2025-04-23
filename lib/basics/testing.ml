module TestInput = Keyboard.MakeInput (struct
  (** [arr] is an array of integers representing a sequence of key codes. *)
  let arr =
    [|
      98;
      113;
      119;
      97;
      115;
      100;
      32;
      10;
      Curses.Key.up;
      Curses.Key.down;
      Curses.Key.left;
      Curses.Key.right;
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

let string_of_list elem_to_string lst =
  "["
  ^ List.fold_left (fun acc el -> " " ^ elem_to_string el ^ ";" ^ acc) "]" lst

let input_responses () =
  let curr_input = TestInput.read_input () in
  let out = [] in
  let rec helper curr lst =
    if curr <> Keyboard.None then curr :: helper (TestInput.read_input ()) lst
    else lst
  in
  helper curr_input out

let run_inputs input_list =
  let default_dungeon = Dungeon.create_test () in
  let default_room = Dungeon.current_room default_dungeon in
  let rec helper = function
    | [] -> default_room
    | h :: t ->
        Game.test_input_handling default_dungeon h;
        helper t
  in
  helper input_list
