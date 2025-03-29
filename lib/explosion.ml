type t = {
  position : Coords.t;
  max_radius : int;
  mutable curr_radius : int;
  mutable in_progress : bool;
}

let create position max_radius =
  { position; max_radius; curr_radius = 0; in_progress = true }

let spread exp =
  if exp.curr_radius = exp.max_radius then exp.in_progress <- false
  else exp.curr_radius <- exp.curr_radius + 1

let tile_is_exploding tile exp =
  exp.in_progress
  &&
  let distance = Coords.euclid_dist exp.position tile in
  exp.curr_radius >= int_of_float (ceil (distance -. 0.5))

let is_in_progress exp = exp.in_progress
