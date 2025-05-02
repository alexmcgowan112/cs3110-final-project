open Connections

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

let tile_is_exploding tile explosions graph =
  List.exists
    (fun exp ->
      exp.in_progress
      &&
      let visited = Hashtbl.create 100 in
      let tiles_in_radius = ref [] in
      let module Bfs = Connections.Search in
      let enqueue v d =
        if d <= exp.curr_radius + 1 then (
          Hashtbl.replace visited v d;
          tiles_in_radius := v :: !tiles_in_radius)
      in
      let rec bfs queue =
        match queue with
        | [] -> ()
        | (v, d) :: rest ->
            if (not (Hashtbl.mem visited v)) && d <= exp.curr_radius + 1 then (
              enqueue v d;
              let neighbors = Connections.G.succ graph v in
              let next = List.map (fun n -> (n, d + 1)) neighbors in
              bfs (rest @ next))
            else bfs rest
      in
      bfs [ (exp.position, 0) ];
      List.exists
        (fun v ->
          Coords.equal v tile
          && exp.curr_radius
             >= int_of_float
                  (ceil (Coords.euclid_dist exp.position tile -. 0.5)))
        !tiles_in_radius)
    explosions

let spread exp =
  if exp.curr_radius = exp.max_radius then exp.in_progress <- false
  else exp.curr_radius <- exp.curr_radius + 1
