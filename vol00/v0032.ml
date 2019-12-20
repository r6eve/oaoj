let rec solve l r =
  try
    let (a, b, c) = Scanf.scanf "%d,%d,%d " (fun a b c -> a, b, c) in
    if a = b then solve (l + 1) r
    else if a*a + b*b = c*c then solve l (r + 1)
    else assert false
  with _ -> (r, l)

let () =
  let (r, l) = solve 0 0 in
  Printf.printf "%d\n%d\n" r l
