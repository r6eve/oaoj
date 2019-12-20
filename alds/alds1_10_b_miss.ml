(* https://topcoder.g.hatena.ne.jp/spaghetti_source/20121208/1354954404 *)
let min_pos a =
  let n = Array.length a in
  let rec doit i (pos, m) =
    if i = n then pos
    else if a.(i) < m then doit (i + 1) (i, a.(i))
    else doit (i + 1) (pos, m) in
  doit 0 (-1, max_int)

let rotate n a =
  let l = Array.length a in
  let rec doit i j m =
    if i <> j then begin
      let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp;
      doit (i + 1) (if j + 1 = l then m else j + 1) (if i + 1 = m then j + 1 else m)
    end in
  doit 0 n n

let cond_p a b c d = b*c*d + a*b*d <= a*c*d + a*b*c

let mcm p n =
  rotate (min_pos p) p;
  let s = Array.make n 0 in
  let k = ref 0 in
  let cost = ref 0 in
  for i = 1 to n - 1 do
    while !k >= 2 && cond_p p.(0) p.(s.(!k-2)) p.(s.(!k-1)) p.(i) do
      cost := !cost + p.(s.(!k-2)) * p.(s.(!k-1)) * p.(i);
      decr k
    done;
    s.(!k) <- i;
    incr k
  done;
  for i = 0 to !k - 2 do
    cost := !cost + p.(0) * p.(s.(i)) * p.(s.(i+1))
  done;
  !cost

let () =
  let n = read_int () in
  let np = n + 1 in
  let p = Array.make np 0 in
  let rec read i =
    if i < n then begin
      let (r, c) = Scanf.scanf "%d %d\n" (fun r c -> (r, c)) in
      p.(i) <- r; p.(i+1) <- c;
      read (i + 1)
    end in
  read 0;
  Printf.printf "%d\n" (mcm p np)
