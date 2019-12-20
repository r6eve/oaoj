let gcd (m, n) =
  let rec doit x y =
    if y <= 0 then x else doit y (x mod y) in
  doit (max m n) (min m n)

let () =
  Scanf.scanf "%d %d\n" (fun a b -> (a, b)) |> gcd |> Printf.printf "%d\n"
