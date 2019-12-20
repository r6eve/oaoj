let lower_bound (a : int array) first last v =
  let rec doit len first =
    if len = 0 then first
    else
      let half = len / 2 in
      let mid = first + half in
      if a.(mid) < v then doit (len - half - 1) (mid + 1)
      else doit half first in
  doit (last - first + 1) first

let () =
  let n = read_int () in
  let dp = Array.make (n + 1) max_int in
  for _ = 0 to n - 1 do
    let a = read_int () in
    dp.(lower_bound dp 0 n a) <- a
  done;
  lower_bound dp 0 n max_int |> Printf.printf "%d\n"
