open OUnit2
open Cs3110_final_project.Keyboard

module Gettable (G : sig
  val arr : int array
end) =
Keyboard (struct
  let arr = G.arr
  let ind = ref 0

  let get () =
    let i = !ind in
    if i < Array.length arr then (
      let v = arr.(i) in
      ind := i + 1;
      v)
    else 1
end)

module Input = Gettable (struct
  let arr = [| 98; 113; 101; 259; 338; 260; 261 |]
end)

let tests =
  "test suite"
  >::: [
         ( "keyboard input test up" >:: fun _ ->
           assert_equal Input.B (Input.read_input ()) );
       ]

let _ = run_test_tt_main tests
