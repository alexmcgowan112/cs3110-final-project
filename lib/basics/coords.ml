type t = {
  x : int;
  y : int;
}

let equal c1 c2 = c1.x = c2.x && c1.y = c2.y
let add c1 c2 = { x = c1.x + c2.x; y = c1.y + c2.y }
let manhattan_dist c1 c2 = abs (c1.x - c2.x) + abs (c1.y - c2.y)
let to_string coord = Printf.sprintf "(%d, %d)" coord.x coord.y

let euclid_dist c1 c2 =
  let dx, dy = (c1.x - c2.x, c1.y - c2.y) in
  sqrt (float_of_int ((dx * dx) + (dy * dy)))

let add_dir coord n = function
  | Keyboard.Up -> { x = coord.x; y = coord.y - n }
  | Keyboard.Down -> { x = coord.x; y = coord.y + n }
  | Keyboard.Left -> { x = coord.x - n; y = coord.y }
  | Keyboard.Right -> { x = coord.x + n; y = coord.y }
  | _ -> coord
