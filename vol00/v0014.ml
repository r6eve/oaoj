let () =
  let f x = x * x in
  try
    while true do
      let d = read_int () in
      let rec doit i a =
        if d*i >= 600 then a
        else d * f (d*i) + a |> doit (i + 1) in
      doit 1 0 |> Printf.printf "%d\n"
    done
  with _ -> ()
