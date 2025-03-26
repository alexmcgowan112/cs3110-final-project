module Keyboard : functor
  (G : sig
     val get : unit -> Curses.attr_t
   end)
  -> sig
  type t =
    | ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | B
    | E
    | Q
    | None

  val read_input : unit -> t
end
