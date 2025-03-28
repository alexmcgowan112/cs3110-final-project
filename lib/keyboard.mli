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

module MakeInput : functor
  (G : sig
     val get : unit -> Curses.attr_t
   end)
  -> sig
  val read_input : unit -> t
  val string_of_input : t -> string
end
