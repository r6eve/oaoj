let gcd m n =
  let rec doit x y =
    if y <= 0 then x else doit y (x mod y) in
  doit (max m n) (min m n)

let ext_euclid a b g =
  let rec doit (x, y, z) (s, t, u) =
    if u = g || u = 0 then (s, t)
    else
      let c = (z - z mod u) / u in
      doit (s, t, u) (x - c*s, y - c*t, z - c*u) in
  doit (1, 0, a) (0, 1, b)

let () =
  let (a, b) = Scanf.scanf "%d %d\n" (fun a b -> (a, b)) in
  let (x, y) = ext_euclid a b (gcd a b) in
  Printf.printf "%d %d\n" x y
