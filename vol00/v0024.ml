let () =
  try
    while true do
      let f = read_float () in
      let y = 4.9 *. (f /. 9.8)**2. |> truncate in
      let rec doit i =
        if y < 5*i - 5 then i
        else doit (i + 1) in
      doit 1 |> Printf.printf "%d\n"
    done
  with _ -> ()
