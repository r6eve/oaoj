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

module H = MakeBinaryHeap(struct type t = (int * int) let compare (_, x) (_, y) = y - x end)

let prim a n =
  let reached_p = Array.make n false in
  let d = Array.make n max_int in
  d.(0) <- 0;
  let que = H.make 500 (0, 0) in
  H.push (0, 0) que;
  while not (H.empty_p que) do
    let (u, _) = H.pop que in
    reached_p.(u) <- true;
    List.iter (fun (v, c) ->
      if not reached_p.(v) && c < d.(v) then begin
        d.(v) <- c;
        H.push (v, c) que;
      end) a.(u);
  done;
  Array.fold_left (fun sum c -> if c = max_int then sum else sum + c) 0 d

let () =
  let n = read_int () in
  let a = Array.make n [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let x = Scanf.scanf " %d" (fun i -> i) in
      if x <> (-1) then a.(i) <- (j, x) :: a.(i);
    done
  done;
  prim a n |> Printf.printf "%d\n"
