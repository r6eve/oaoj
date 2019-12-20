let fib_dp n =
  if n < 2 then 1
  else
    let rec doit i x y =
      if i = n then y
      else doit (i + 1) y (x + y) in
    doit 2 1 2

let () = read_int () |> fib_dp |> Printf.printf "%d\n"
