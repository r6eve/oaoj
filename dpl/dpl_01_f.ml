type t = { v : int; w : int }

let solve n w a =
  let z = 100*n in
  let t = Array.make (z + 1) (w + 1) in
  t.(0) <- 0;
  for i = 0 to n - 1 do
    for j = z downto a.(i).v do
      if t.(j - a.(i).v) + a.(i).w < t.(j) then
        t.(j) <- t.(j - a.(i).v) + a.(i).w
    done
  done;
  let rec doit i =
    if i < 0 then assert false
    else if t.(i) <= w then i
    else doit (i - 1) in
  doit z

let () =
  let (n, w) = Scanf.scanf "%d %d " (fun n w -> n, w) in
  Array.init n (fun _ -> Scanf.scanf "%d %d " (fun v w -> { v; w }))
  |> solve n w |> Printf.printf "%d\n"
