let prim a n =
  let reached_p = Array.make n false in
  let d = Array.make n max_int in
  d.(0) <- 0;
  let rec doit () =
    let u =
      Array.fold_left (fun (i, m, u) p ->
        if not p && d.(i) < m then (i + 1, d.(i), i)
        else (i + 1, m, u))
        (0, max_int, (-1)) reached_p
      |> (fun (_, _, u) -> u) in
    if u = (-1) then ()
    else begin
      reached_p.(u) <- true;
      Array.iteri (fun j p ->
        if a.(u).(j) <> (-1) && not p && d.(j) > a.(u).(j) then d.(j) <- a.(u).(j))
        reached_p;
      doit ()
    end in
  doit ();
  Array.fold_left (fun sum c -> if c <> max_int then sum + c else sum) 0 d

let () =
  let n = read_int () in
  let a = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- Scanf.scanf " %d" (fun x -> x);
    done
  done;
  prim a n |> Printf.printf "%d\n"
