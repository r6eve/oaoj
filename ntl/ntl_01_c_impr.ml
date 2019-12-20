let gcd m n =
  let rec doit x y =
    if y <= 0 then x else doit y (x mod y) in
  doit (max m n) (min m n)

let lcm m n = m / (gcd m n) * n

let read_n () = Scanf.scanf "%d " (fun i -> i)

let () =
  let n = read_n () in
  let rec doit i acc =
    if i = n then acc
    else read_n () |> lcm acc |> doit (i + 1) in
  read_n () |> doit 1 |> Printf.printf "%d\n"
