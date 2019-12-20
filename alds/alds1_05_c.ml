let pi = 4. *. atan 1.

let to_radian degree = pi *. degree /. 180.

type point = { x : float; y : float }

let print_point e = Printf.printf "%f %f\n" e.x e.y

let rec koch n a b =
  if n = 0 then () else
  let s =
    let x = (2. *. a.x +. b.x) /. 3. in
    let y = (2. *. a.y +. b.y) /. 3. in
    { x; y } in
  let t =
    let x = (a.x +. 2. *. b.x) /. 3. in
    let y = (a.y +. 2. *. b.y) /. 3. in
    { x; y } in
  let u =
    let x = (t.x -. s.x) *. cos (to_radian 60.) -. (t.y -. s.y) *. sin (to_radian 60.) +. s.x in
    let y = (t.x -. s.x) *. sin (to_radian 60.) +. (t.y -. s.y) *. cos (to_radian 60.) +. s.y in
    { x; y } in
  koch (n - 1) a s;
  print_point s;
  koch (n - 1) s u;
  print_point u;
  koch (n - 1) u t;
  print_point t;
  koch (n - 1) t b

let () =
  let n = read_int () in
  let p1 = { x = 0.; y = 0. } in
  let p2 = { x = 100.; y = 0. } in
  print_point p1;
  koch n p1 p2;
  print_point p2
