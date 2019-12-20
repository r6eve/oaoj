let solve d n m =
  let dp = Array.make_matrix m (n + 1) max_int in
  for i = 0 to m - 1 do dp.(i).(0) <- 0 done;
  for j = 1 to n do dp.(0).(j) <- j done;
  for i = 1 to m - 1 do
    for j = 1 to n do
      dp.(i).(j) <-
        if j - d.(i) < 0 then dp.(i-1).(j)
        else min dp.(i-1).(j) (dp.(i).(j - d.(i)) + 1)
    done
  done;
  dp.(m-1).(n)

let () =
  let n, m = Scanf.scanf "%d %d " (fun n m -> n, m) in
  let d = Array.init m (fun _ -> Scanf.scanf "%d " (fun i -> i)) in
  solve d n m |> Printf.printf "%d\n"
