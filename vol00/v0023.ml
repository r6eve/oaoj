let () =
  let n = read_int () in
  for _ = 0 to n - 1 do
    let (xa, ya, ra, xb, yb, rb) =
      Scanf.scanf "%f %f %f %f %f %f " (fun xa ya ra xb yb rb ->
        xa, ya, ra, xb, yb, rb) in
    let d = (xa -. xb)**2. +. (ya -. yb)**2. |> sqrt in
    print_endline
      (if ra -. rb > d then "2"
       else if rb -. ra > d then "-2"
       else if ra +. rb < d then "0"
       else "1")
  done
