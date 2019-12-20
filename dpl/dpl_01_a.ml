let solve n m d =
  let dp = Array.init (n + 1) (fun i -> i) in
  for i = 1 to m - 1 do
    for j = d.(i) to n do
      if dp.(j - d.(i)) + 1 < dp.(j) then
        dp.(j) <- dp.(j - d.(i)) + 1;
    done
  done;
  dp.(n)

let () =
  let (n, m) = Scanf.scanf "%d %d " (fun n m -> n, m) in
  Array.init m (fun _ -> Scanf.scanf "%d " (fun i -> i))
  |> solve n m |> Printf.printf "%d\n"
