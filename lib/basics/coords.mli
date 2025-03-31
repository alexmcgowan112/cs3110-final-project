type t = {
  x : int;
  y : int;
}
(** AF: [{x; y}] represents a coordinate in a 2D space with [x] and [y]
    coordinates. RI: [x] and [y] are non-negative integers. *)

val equal : t -> t -> bool
(** [equal c1 c2] returns true if the coordinates [c1] and [c2] are equal. *)

val add : t -> t -> t
(** [add c1 c2] returns a new coordinate that is the sum of coordinates [c1] and
    [c2]. *)

val add_dir : t -> int -> Keyboard.t -> t
(** [add_dir coord n dir] returns a new coordinate that is the result of moving
    from coordinate [coord] in direction [dir] by distance [n]. *)

val manhattan_dist : t -> t -> int
(** [manhattan_dist c1 c2] returns the Manhattan distance between coordinates
    [c1] and [c2]. *)

val euclid_dist : t -> t -> float
(** [euclid_dist c1 c2] returns the Euclidean distance between coordinates [c1]
    and [c2]. *)

val to_string : t -> string
(** [to_string coord] is the string representation of the coordinate (x, y).*)
