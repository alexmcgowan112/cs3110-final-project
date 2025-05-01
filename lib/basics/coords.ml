type t = {
  x : int;
  y : int;
}

let compare c1 c2 =
  if c1.x = c2.x then if c1.y = c2.y then 0 else if c1.x < c2.x then -1 else 1
  else if c1.y < c2.y then -1
  else 1

let equal c1 c2 = compare c1 c2 = 0
let hash = Hashtbl.hash
let add c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y }

let add_dir coord n = function
  | Keyboard.Up -> { coord with y = coord.y - n }
  | Keyboard.Down -> { coord with y = coord.y + n }
  | Keyboard.Left -> { coord with x = coord.x - n }
  | Keyboard.Right -> { coord with x = coord.x + n }
  | _ -> failwith "add_dir expects a direction"

let manhattan_dist c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)

let euclid_dist c1 c2 =
  let dx, dy = (c1.x - c2.x, c1.y - c2.y) in
  sqrt (float_of_int ((dx * dx) + (dy * dy)))

(* chebyshev distance basically lets us check if an enemy is immediatly next to
   a player in any direction *)
let chebyshev_dist c1 c2 = max (abs (c1.x - c2.x)) (abs (c1.y - c2.y))
let to_string coord = Printf.sprintf "(%d, %d)" coord.x coord.y
