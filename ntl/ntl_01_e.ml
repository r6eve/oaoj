let rec ext_euclid a b =
  if b = 0 then (1, 0)
  else
    let (x, y) = ext_euclid b (a mod b) in
    (y, x - (a / b) * y)

let () =
  let (a, b) = Scanf.scanf "%d %d\n" (fun a b -> (a, b)) in
  let (x, y) = ext_euclid a b in
  Printf.printf "%d %d\n" x y
