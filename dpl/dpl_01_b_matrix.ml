type t = { v : int; w : int }

let max (x : int) y = if x > y then x else y

let solve a n w =
  let dp = Array.make_matrix (n + 1) (w + 1) 0 in
  for i = 1 to n do
    for j = 1 to w do
      dp.(i).(j) <-
        if j - a.(i-1).w < 0 then dp.(i-1).(j)
        else max dp.(i-1).(j) (dp.(i-1).(j - a.(i-1).w) + a.(i-1).v);
    done
  done;
  dp.(n).(w)

let () =
  let (n, w) = Scanf.scanf "%d %d " (fun n w -> n, w) in
  let a = Array.init n (fun _ -> Scanf.scanf "%d %d " (fun v w -> { v; w })) in
  solve a n w |> Printf.printf "%d\n"
