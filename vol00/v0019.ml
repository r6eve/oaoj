let () =
  let n = read_int () in
  let rec doit i acc =
    if i = 0 then acc
    else doit (i - 1) (acc * i) in
  doit n 1 |> Printf.printf "%d\n"
