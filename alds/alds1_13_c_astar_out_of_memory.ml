(* out of memory *)
type puzzle = { board : int array; space : int; lower : int; turn : int }
type state = { puzzle : puzzle; estimated : int }

module MakeBinaryHeap (M : sig type t val compare : t -> t -> int end) = struct

  type t = { node : M.t array; mutable size : int }

  let make n (init : M.t) = { node = Array.make n init; size = 0 }

  let empty_p t = if t.size = 0 then true else false

  let pop t =
    let rec max_heapify i =
      let l = 2*i in
      let r = 2*i + 1 in
      let m = if l <= t.size && M.compare t.node.(l) t.node.(i) > 0 then l else i in
      let m = if r <= t.size && M.compare t.node.(r) t.node.(m) > 0 then r else m in
      if m = i then ()
      else begin
        let tmp = t.node.(i) in
        t.node.(i) <- t.node.(m);
        t.node.(m) <- tmp;
        max_heapify m
      end in
    let ret = t.node.(1) in
    t.node.(1) <- t.node.(t.size);
    t.size <- t.size - 1;
    max_heapify 1;
    ret

  let push x t =
    let parent i = int_of_float (floor (float_of_int i) /. 2.) in
    let rec doit i =
      let p = parent i in
      if i <= 1 || M.compare t.node.(p) t.node.(i) >= 0 then ()
      else begin
        let tmp = t.node.(i) in
        t.node.(i) <- t.node.(p);
        t.node.(p) <- tmp;
        doit p
      end in
    t.size <- t.size + 1;
    t.node.(t.size) <- x;
    doit t.size

end

module H = MakeBinaryHeap(struct type t = state let compare x y = y.estimated - x.estimated end)

let (n1, n2) = (4, 16)
let mdt = Array.make_matrix n2 n2 0
let dx = [| 0; -1; 0; 1 |]
let dy = [| 1; 0; -1; 0 |]

let astar board space =
  let (_, lower) =
    Array.fold_left (fun (i, sum) e ->
      (i + 1, if e = n2 then sum else sum + mdt.(i).(e-1)))
      (0, 0) board in
  let pzl = { board; space; lower; turn = 0 } in
  let initial = { puzzle = pzl; estimated = pzl.lower } in
  let que = H.make 1000000 { puzzle = { board = [||]; space = 0; lower = 0; turn = 0 }; estimated = 0 } in
  H.push initial que;
  let reached_p = Hashtbl.create ~random:true 1000000 in
  while not (H.empty_p que) do
    let st = H.pop que in
    let { board = b; space = s; lower = m; turn = c } = st.puzzle in
    let u = { board = Array.copy b; space = s; lower = m; turn = c } in
    if u.lower == 0 then begin
      Printf.printf "%d\n" u.turn;
      exit 0;
    end else begin
      Hashtbl.add reached_p u true;
      let (sx, sy) = (u.space / n1, u.space mod n1) in
      for r = 0 to 3 do
        let (tx, ty) = (sx + dx.(r), sy + dy.(r)) in
        if tx < 0 || ty < 0 || tx >= n1 || ty >= n1 then ()
        else begin
          let { board; lower; turn } = u in
          let board = Array.copy board in
          let (space, num) = (tx * n1 + ty, sx * n1 + sy) in
          let lower = lower - mdt.(space).(board.(space)-1) + mdt.(num).(board.(space)-1) in
          let t = board.(space) in
          board.(space) <- board.(num);
          board.(num) <- t;
          if not (Hashtbl.mem reached_p { board; space; lower; turn }) then begin
            let v = { board; space; lower; turn = turn + 1 } in
            H.push { puzzle = v; estimated = v.turn + v.lower } que
          end
        end
      done
    end
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
  doit 0 0 |> astar board
