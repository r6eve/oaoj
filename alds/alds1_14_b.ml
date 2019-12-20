let pn = 1000000007

let pow x n =
  let rec doit x n acc =
    if n = 0 then acc
    else doit (x * x) (n / 2) (if n mod 2 = 0 then acc else acc * x) in
  doit x n 1

let fold_left f init s n =
  let rec doit i acc =
    if i = n then acc
    else doit (i + 1) (f acc s.[i]) in
  doit 0 init

let () =
  let p = read_line () in
  let t = read_line () in
  let n = String.length p in
  let m = String.length t in
  let h = Array.make (n + 1) 0 in
  let hash x c = x * pn + Char.code c in
  String.iteri (fun i c -> h.(i+1) <- hash h.(i) c) p;
  let v = fold_left hash 0 t m in
  let x = pow pn m in
  for i = 0 to n - m do
    if h.(i+m) - h.(i)*x = v then Printf.printf "%d\n" i;
  done
