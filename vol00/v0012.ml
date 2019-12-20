let () =
  try
    while true do
      let (x1, y1, x2, y2, x3, y3, xp, yp) =
        Scanf.scanf "%f %f %f %f %f %f %f %f " (fun x1 y1 x2 y2 x3 y3 xp yp ->
          x1, y1, x2, y2, x3, y3, xp, yp) in
      let pa = (x1 -. x3) *. (yp -. y1) -. (y1 -. y3) *. (xp -. x1) in
      let pb = (x2 -. x1) *. (yp -. y2) -. (y2 -. y1) *. (xp -. x2) in
      let pc = (x3 -. x2) *. (yp -. y3) -. (y3 -. y2) *. (xp -. x3) in
      print_endline
        (if pa > 0. && pb > 0. && pc > 0. || pa < 0. && pb < 0. && pc < 0. then "YES" else "NO")
    done
  with _ -> ()
