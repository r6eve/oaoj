let z = 1000000007

let pow (x, n) =
  let rec doit x n acc =
    if n = 0 then acc
    else if n mod 2 = 0 then doit (x * x mod z) (n / 2) acc
    else doit (x * x mod z) (n / 2) (acc * x mod z) in
  doit x n 1

let () =
  Scanf.scanf "%d %d\n" (fun m n -> (m, n)) |> pow |> Printf.printf "%d\n"
