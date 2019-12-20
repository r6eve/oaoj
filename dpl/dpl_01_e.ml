let min (x : int) y = if x < y then x else y

let levenshtein_distance (x, y) =
  if x = y then 0
  else
    let m = String.length x in
    let n = String.length y in
    if m = 0 then n
    else if n = 0 then m
    else begin
      let s = Array.init (n + 1) (fun i -> i) in
      let t = Array.make (n + 1) 0 in
      for i = 0 to m - 1 do
        t.(0) <- i + 1;
        for j = 0 to n - 1 do
          t.(j+1) <- min
            (s.(j) + if x.[i] = y.[j] then 0 else 1)
            (min t.(j) s.(j+1) + 1);
        done;
        Array.iteri (fun i e -> s.(i) <- e) t
      done;
      t.(n)
    end

let () =
  Scanf.scanf "%s %s " (fun x y -> x, y)
  |> levenshtein_distance
  |> Printf.printf "%d\n"
