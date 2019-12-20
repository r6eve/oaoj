let prim a n =
  let reached_p = Array.make n false in
  let d = Array.make n max_int in
  d.(0) <- 0;
  let from = Array.make n (-1) in
  let rec doit () =
    let u =
      Array.fold_left (fun (j, m, u) p ->
        if not p && d.(j) < m then (j + 1, d.(j), j) else (j + 1, m, u))
        (0, max_int, (-1)) reached_p
      |> (fun (_, _, u) -> u) in
    if u = (-1) then ()
    else begin
      reached_p.(u) <- true;
      Array.iteri (fun j p ->
        if a.(u).(j) <> (-1) && not p && a.(u).(j) < d.(j) then begin
          d.(j) <- a.(u).(j);
          from.(j) <- u;
        end) reached_p;
      doit ()
    end in
  doit ();
  Array.fold_left (fun (j, sum) i ->
    (j + 1, if i = (-1) then sum else sum + a.(i).(j)))
    (0, 0) from
  |> snd

let () =
  let n = read_int () in
  let a = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- Scanf.scanf " %d" (fun i -> i)
    done
  done;
  prim a n |> Printf.printf "%d\n"
