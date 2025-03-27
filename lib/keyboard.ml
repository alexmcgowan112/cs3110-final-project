type t =
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | B
  | E
  | Q
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
      | 101 -> E
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
    | E -> "E"
    | B -> "B"
    | None -> "None"
end
