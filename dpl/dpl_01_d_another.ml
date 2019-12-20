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
  let l = ref 0 in
  for _ = 0 to n - 1 do
    let a = read_int () in
    let x = lower_bound dp 0 n a in
    dp.(x) <- a;
    if x + 1 > !l then l := x + 1;
  done;
  Printf.printf "%d\n" !l
