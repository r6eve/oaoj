let () =
  let n = read_int () in
  for _ = 0 to n - 1 do
    let (x1, y1, x2, y2, x3, y3) = Scanf.scanf "%f %f %f %f %f %f " (fun x1 y1 x2 y2 x3 y3 -> x1, y1, x2, y2, x3, y3) in
    let a11 = 2. *. (x2 -. x1) in
    let a12 = 2. *. (y2 -. y1) in
    let a21 = 2. *. (x3 -. x2) in
    let a22 = 2. *. (y3 -. y2) in
    let b1 = x2 *. x2 +. y2 *. y2 -. x1 *. x1 -. y1 *. y1 in
    let b2 = x3 *. x3 +. y3 *. y3 -. x2 *. x2 -. y2 *. y2 in
    let det = a11 *. a22 -. a12 *. a21 in
    let x = a22 /. det *. b1 -. a12 /. det *. b2 in
    let y = a11 /. det *. b2 -. a21 /. det *. b1 in
    (x1 -. x) ** 2. +. (y1 -. y) ** 2.  |> sqrt
    |> Printf.printf "%.3f %.3f %.3f\n" x y
  done
