let mul a b =
  let na0 = Array.length a in
  let nb0 = Array.length b in
  let nb1 = Array.length b.(0) in
  let c = Array.make_matrix na0 nb1 0 in
  for i = 0 to na0 - 1 do
    for k = 0 to nb0 - 1 do
      for j = 0 to nb1 - 1 do
        c.(i).(j) <- c.(i).(j) + a.(i).(k) * b.(k).(j)
      done
    done
  done;
  c

let fib n =
  let a = Array.make_matrix 2 2 0 in
  a.(0).(0) <- 1; a.(0).(1) <- 1;
  a.(1).(0) <- 1; a.(1).(1) <- 0;
  let b = Array.make_matrix 2 2 0 in
  b.(0).(0) <- 1; b.(0).(1) <- 0;
  b.(1).(0) <- 0; b.(1).(1) <- 1;
  let rec doit n a b =
    if n = 0 then b
    else doit (n / 2) (mul a a) (if n mod 2 = 0 then b else mul b a) in
  let c = doit (n + 1) a b in
  c.(1).(0)

let () = read_int () |> fib |> Printf.printf "%d\n"
