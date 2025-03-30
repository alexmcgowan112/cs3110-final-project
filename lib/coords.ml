type t = {
  x : int;
  y : int;
}

let equal c1 c2 = c1.x = c2.x && c1.y = c2.y
let add c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y }
let manhattan_dist c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)

let euclid_dist c1 c2 =
  let dx, dy = (c1.x - c2.x, c1.y - c2.y) in
  sqrt (float_of_int ((dx * dx) + (dy * dy)))
