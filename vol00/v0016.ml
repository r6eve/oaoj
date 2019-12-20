let pi = 4. *. atan 1.

let degree_to_radian d = pi *. d /. 180.

let () =
  let rec doit (x, y) r =
    let (d, t) = Scanf.scanf "%f,%f " (fun d t -> d, t) in
    if d = 0. && t = 0. then (truncate x, truncate y)
    else
      let z = degree_to_radian r in
      doit (x +. d *. cos z, y +. d *. sin z) (r -. t) in
  let (x, y) = doit (0., 0.) 90. in
  Printf.printf "%d\n%d\n" x y
