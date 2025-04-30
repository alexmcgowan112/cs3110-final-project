type t = {
  position : Coords.t;
  max_radius : int;
  mutable curr_radius : int;
  mutable in_progress : bool;
}
(** [{position; max_radius; curr_radius; in_progress}] represents an explosion
    at [position]. The explosion currently has [curr_radius] and will grow to a
    maximum size of [max_radius]. If the explosion is still ongoing, it will be
    [in_progress]. RI: [curr_radius <= max_radius] *)

let create position max_radius =
  { position; max_radius; curr_radius = 0; in_progress = true }

let is_in_progress exp = exp.in_progress

let tile_is_exploding tile explosions =
  List.exists
    (fun exp ->
      exp.in_progress
      &&
      let distance = Coords.euclid_dist exp.position tile in
      exp.curr_radius >= int_of_float (ceil (distance -. 0.5)))
    explosions

let spread exp =
  if exp.curr_radius = exp.max_radius then exp.in_progress <- false
  else exp.curr_radius <- exp.curr_radius + 1
