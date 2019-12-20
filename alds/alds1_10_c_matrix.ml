let max (x : int) y = if x > y then x else y

let lcs x y =
  let m = String.length x in
  let n = String.length y in
  let dp = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 1 to m do
    for j = 1 to n do
      dp.(i).(j) <-
        if x.[i-1] = y.[j-1] then dp.(i-1).(j-1) + 1
        else max dp.(i-1).(j) dp.(i).(j-1);
    done
  done;
  dp.(m).(n)

let () =
  let q = read_int () in
  for _ = 0 to q - 1 do
    let x = read_line () in
    let y = read_line () in
    lcs x y |> print_int;
    print_newline ();
  done
