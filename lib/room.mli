type t

type direction =
  | Up
  | Down
  | Left
  | Right

val new_room : unit -> t
val move_player : t -> direction -> unit
val to_string : t -> string
