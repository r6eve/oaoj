let (n1, n2) = (4, 16)
let n_limit = 100
let mdt = Array.make_matrix n2 n2 0
let dx = [| 0; -1; 0; 1 |]
let dy = [| 1; 0; -1; 0 |]

type puzzle = { board : int array; mutable space : int; mutable md : int }

let dfs puzzle limit =
  let rec doit i prev pzl =
    if pzl.md = 0 then begin
      Printf.printf "%d\n" limit;
      exit 0;
    end else if i + pzl.md > limit then ()
    else
      let (sx, sy) = (pzl.space / n1, pzl.space mod n1) in
      for r = 0 to 3 do
        let (tx, ty) = (sx + dx.(r), sy + dy.(r)) in
        if tx < 0 || ty < 0 || tx >= n1 || ty >= n1 then ()
        else if max prev r - min prev r = 2 then ()
        else begin
          let (space, num) = (tx * n1 + ty, sx * n1 + sy) in
          let oldmd = pzl.md in
          pzl.md <- pzl.md - mdt.(space).(pzl.board.(space)-1) + mdt.(num).(pzl.board.(space)-1);
          let olds = pzl.board.(space) in
          let oldn = pzl.board.(num) in
          pzl.board.(space) <- oldn;
          pzl.board.(num) <- olds;
          let oldspace = pzl.space in
          pzl.space <- space;
          doit (i + 1) r pzl;
          pzl.board.(space) <- olds;
          pzl.board.(num) <- oldn;
          pzl.space <- oldspace;
          pzl.md <- oldmd;
        end
      done in
  doit 0 (-n_limit) puzzle

let idastar board space =
  let (_, md) =
    Array.fold_left (fun (i, sum) e ->
      (i + 1, if e = n2 then sum else sum + mdt.(i).(e-1)))
      (0, 0) board in
  let pzl = { board; space; md } in
  for limit = pzl.md to n_limit do
    dfs pzl limit;
  done;
  assert false

let () =
  for i = 0 to n2 - 1 do
    for j = 0 to n2 - 1 do
      mdt.(i).(j) <- abs (i / n1 - j / n1) + abs (i mod n1 - j mod n1)
    done
  done;
  let board = Array.make n2 0 in
  let rec doit i space =
    if i = n2 then space
    else begin
      let x = Scanf.scanf "%d " (fun i -> i) in
      let (x, space) = if x = 0 then (n2, i) else (x, space) in
      board.(i) <- x;
      doit (i + 1) space
    end in
  doit 0 0 |> idastar board
