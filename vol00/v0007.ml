let () =
  let rec doit i x =
    if i = 0 then x
    else 1000. *. (x *. 1.05 /. 1000. |> ceil) |> floor |> doit (i - 1) in
  doit (read_int ()) 100000. |> Printf.printf "%.0f\n"
