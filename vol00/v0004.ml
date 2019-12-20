let () =
  try
    while true do
      let (a, b, c, d, e, f) = Scanf.scanf "%f %f %f %f %f %f " (fun a b c d e f -> a, b, c, d, e, f) in
      let s = a *. e -. b *. d in
      let x = (e *. c -. b *. f) /. s in
      let y = (a *. f -. d *. c) /. s in
      Printf.printf "%.3f %.3f\n" (x +. 0.) (y +. 0.)
    done
  with _ -> ()
