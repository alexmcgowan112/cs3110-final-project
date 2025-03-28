type t =
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | B
  | Q
  | W
  | A
  | S
  | D
  | None

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
      | 119 -> W
      | 97 -> A
      | 115 -> S
      | 100 -> D
      | code when code = Key.up -> ArrowUp
      | code when code = Key.down -> ArrowDown
      | code when code = Key.left -> ArrowLeft
      | code when code = Key.right -> ArrowRight
      | _ -> None
    in
    result

  let string_of_input = function
    | ArrowUp -> "Up"
    | ArrowDown -> "Down"
    | ArrowLeft -> "Left"
    | ArrowRight -> "Right"
    | Q -> "Q"
    | B -> "B"
    | W -> "W"
    | A -> "A"
    | S -> "S"
    | D -> "D"
    | None -> "None"
end
