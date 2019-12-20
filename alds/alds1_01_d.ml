let () =
  let n = read_int () in
  let rec doit i m ans =
    if i = n then ans
    else
      let r = read_int () in
      doit (i + 1) (min m r) (max ans (r - m)) in
  doit 1 (read_int ()) min_int |> Printf.printf "%d\n"
