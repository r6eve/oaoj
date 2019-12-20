module MakeBinaryHeap (M : sig type t val compare : t -> t -> int end) = struct

  type t = { node : M.t array; mutable size : int }

  let make n (init : M.t) = { node = Array.make n init; size = 0 }

  let empty_p t = if t.size = 0 then true else false

  let pop t =
    if t.size <= 0 then failwith "out of size" else
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

type puzzle = { board : int array; space : int; moved: int; lower : int; turn : int }
type ss = { puzzle : puzzle; estimated : int }

module H = MakeBinaryHeap(struct type t = ss let compare x y = y.estimated - x.estimated end)

let (n1, n2) = (4, 16)

let goal = Array.init n2 (fun i -> (i + 1) mod n2)

let area = Array.make n2 []

let md = Array.make_matrix n2 n2 0

let initialize_area () =
  for i = 0 to n2 - 1 do
    let lst = [] in
    let lst = if i + n1 < n2 then (i + n1) :: lst else lst in
    let lst = if (i + 1) mod n1 <> 0 then (i + 1) :: lst else lst in
    let lst = if i - 1 >= 0 && (i - 1) mod n1 <> n1 - 1 then (i - 1) :: lst else lst in
    let lst = if i - n1 >= 0 then (i - n1) :: lst else lst in
    area.(i) <- lst;
  done

let initialize_md () =
  for i = 0 to n2 - 2 do
    for j = 0 to n2 - 1 do
      md.(i+1).(j) <- abs (i / n1 - j / n1) + abs (i mod n1 - j mod n1)
    done
  done

let astar board space lower =
  let reached_p = Hashtbl.create ~random:true 100000 in
  let que = H.make 10000000 { puzzle = { board=[||]; space=0; moved=0; lower=0; turn=0 }; estimated = 0} in
  H.push { puzzle = { board; space; moved=(-1); lower; turn=0 }; estimated = lower } que;
  while not (H.empty_p que) do
    let ss = H.pop que in
    let { puzzle = pzl } = ss in
    if pzl.lower = 0 then begin
      Printf.printf "%d\n" pzl.turn;
      exit 0;
    end else begin
      Hashtbl.add reached_p { board=pzl.board; space=pzl.space; moved=pzl.moved; lower=pzl.lower; turn=pzl.turn; } true;
      List.iter (fun j ->
        let x = pzl.board.(j) in
        if x = pzl.moved then ()
        else begin
          let lower = pzl.lower - md.(x).(j) + md.(x).(pzl.space) in
          let board = Array.copy pzl.board in
          board.(j) <- 0;
          board.(pzl.space) <- x;
          if not (Hashtbl.mem reached_p { board; space = j; moved = x; lower; turn=pzl.turn }) then begin
            let pzl = { board; space = j; moved = x; lower; turn = pzl.turn + 1 } in
            H.push { puzzle = pzl; estimated = pzl.turn + pzl.lower } que;
          end;
        end)
      area.(pzl.space);
    end
  done;
  assert false

let findi a x =
  let n = Array.length a in
  let rec doit i =
    if i = n then assert false
    else if a.(i) = x then i
    else doit (i + 1) in
  doit 0

let () =
  initialize_area ();
  initialize_md ();
  let a = Array.init n2 (fun _ -> Scanf.scanf "%d " (fun i -> i)) in
  let space = findi a 0 in
  Array.fold_left (fun (i, sum) e -> (i + 1, sum + md.(e).(i))) (0, 0) a
  |> snd
  |> astar a space
