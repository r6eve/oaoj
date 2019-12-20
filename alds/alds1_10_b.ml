let mcm p n =
  let dp = Array.make_matrix (n + 1) (n + 1) max_int in
  for i = 1 to n do
    dp.(i).(i) <- 0;
  done;
  for d = 1 to n - 1 do
    for i = 1 to n - d do
      let j = i + d in
      for r = i to j - 1 do
        dp.(i).(j) <- min dp.(i).(j) (dp.(i).(r) + dp.(r+1).(j) + p.(i-1)*p.(r)*p.(j))
      done
    done
  done;
  dp.(1).(n)

let () =
  let n = read_int () in
  let p = Array.make (n + 1) 0 in
  for i = 0 to n - 1 do
    let (r, c) = Scanf.scanf "%d %d\n" (fun r c -> (r, c)) in
    p.(i) <- r;
    p.(i+1) <- c;
  done;
  mcm p n |> Printf.printf "%d\n"
