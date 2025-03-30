type t =
  | Up
  | Down
  | Left
  | Right
  | B
  | Q
  | None
  | Space

let to_string = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Q -> "Q"
  | B -> "B"
  | Space -> "Space"
  | None -> "None"

module MakeInput (G : sig
  val get : unit -> int
end) =
struct
  open Curses

  let read_input () =
    let result =
      match G.get () with
      | 98 -> B
      | 113 -> Q
      | 119 -> Up (* W *)
      | 97 -> Left (* A *)
      | 115 -> Down (* S *)
      | 100 -> Right (* D *)
      | 32 -> Space (* Space *)
      | code when code = Key.up -> Up
      | code when code = Key.down -> Down
      | code when code = Key.left -> Left
      | code when code = Key.right -> Right
      | _ -> None
    in
    result
end
