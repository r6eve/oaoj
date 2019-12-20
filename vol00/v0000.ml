let () =
  for i = 0 to 9 do
    for j = 0 to 9 do
      Printf.printf "%dx%d=%d\n" i j (i * j)
    done
  done
