type t = {
  x : int;
  y : int;
}

val equal : t -> t -> bool
val add : t -> t -> t
val manhattan_dist : t -> t -> int
val euclid_dist : t -> t -> float
