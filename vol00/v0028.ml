let () =
  let a = Array.make 101 0 in
  try
    while true do
      let n = read_int () in
      a.(n) <- a.(n) + 1
    done
  with _ ->
    let m = Array.fold_left (fun v e -> max v e) 0 a in
    Array.iteri (fun i e -> if e = m then Printf.printf "%d\n" i) a
