let gcd x y =
  let rec doit x y =
    if y = 0 then x
    else doit y (x mod y) in
  doit (max x y) (min x y)

let lcm x y g = x / g * y

let () =
  try
    while true do
      let (x, y) = Scanf.scanf "%d %d " (fun x y -> x, y) in
      let g = gcd x y in
      lcm x y g |> Printf.printf "%d %d\n" g
    done
  with _ -> ()
