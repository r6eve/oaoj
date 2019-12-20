type t = { v : int; w : int }

let solve n w d =
  let dp = Array.make (w + 1) 0 in
  for i = 0 to n - 1 do
    for j = d.(i).w to w do
      if dp.(j - d.(i).w) + d.(i).v > dp.(j) then
        dp.(j) <- dp.(j - d.(i).w) + d.(i).v;
    done
  done;
  dp.(w)

let () =
  let (n, w) = Scanf.scanf "%d %d " (fun n w -> n, w) in
  Array.init n (fun _ -> Scanf.scanf "%d %d " (fun v w -> { v; w }))
  |> solve n w |> Printf.printf "%d\n"
