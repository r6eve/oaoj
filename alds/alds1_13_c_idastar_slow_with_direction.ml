let (n1, n2) = (3, 9)
(* let (n1, n2) = (4, 16) *)
let n_limit = 100
let mdt = Array.make_matrix n2 n2 0
let path = Array.make n_limit 0
let dx = [| 0; -1; 0; 1 |]
let dy = [| 1; 0; -1; 0 |]
let dir = [| "r"; "u"; "l"; "d" |]

type puzzle = { tbl : int array; space : int; md : int }

let dfs puzzle limit =
  let rec doit depth prev pzl =
    if pzl.md = 0 then true
    else if depth + pzl.md > limit then false
    else begin
      let (sx, sy) = (pzl.space / n1, pzl.space mod n1) in
      let { tbl; md } = pzl in
      let swap tbl i j = let t = tbl.(i) in tbl.(i) <- tbl.(j); tbl.(j) <- t in
      let rec move r =
        if r = 4 then false
        else
          let (tx, ty) = (sx + dx.(r), sy + dy.(r)) in
          if tx < 0 || ty < 0 || tx >= n1 || ty >= n1 then move (r + 1)
          else if max prev r - min prev r = 2 then move (r + 1)
          else begin
            let (space, num) = (tx * n1 + ty, sx * n1 + sy) in
            let md = md - mdt.(space).(tbl.(space)-1) + mdt.(num).(tbl.(space)-1) in
            let tbl = Array.copy tbl in
            swap tbl space num;
            if doit (depth + 1) r { tbl; space; md } then begin
              path.(depth) <- r;
              true
            end else move (r + 1)
          end in
      move 0
    end in
  doit 0 (-n_limit) puzzle

let idastar tbl space =
  let (_, md) =
    Array.fold_left (fun (i, sum) e ->
      (i + 1, if e = n2 then sum else sum + mdt.(i).(e-1)))
      (0, 0) tbl in
  let pzl = { tbl; space; md } in
  let rec doit limit =
    if limit > n_limit then assert false
    else if dfs pzl limit then begin
      let str = ref "" in
      for i = 0 to limit - 1 do
        str := !str ^ dir.(path.(i));
      done;
      !str
    end else doit (limit + 1) in
  doit pzl.md

let () =
  for i = 0 to n2 - 1 do
    for j = 0 to n2 - 1 do
      mdt.(i).(j) <- abs (i / n1 - j / n1) + abs (i mod n1 - j mod n1)
    done
  done;
  let tbl = Array.make n2 0 in
  let rec doit i space =
    if i = n2 then space
    else begin
      let x = Scanf.scanf "%d " (fun i -> i) in
      let (x, space) = if x = 0 then (n2, i) else (x, space) in
      tbl.(i) <- x;
      doit (i + 1) space
    end in
  doit 0 0 |> idastar tbl |> String.length |> Printf.printf "%d\n"
