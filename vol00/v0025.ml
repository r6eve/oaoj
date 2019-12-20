let () =
  try
    while true do
      let s = Scanf.scanf "%d %d %d %d " (fun a b c d -> [a; b; c; d]) in
      let t = Scanf.scanf "%d %d %d %d " (fun a b c d -> [a; b; c; d]) in
      let h = List.fold_left2 (fun acc a b -> acc + if a = b then 1 else 0) 0 s t in
      List.fold_left (fun b a -> b + if List.mem a t then 1 else 0) 0 s - h
      |> Printf.printf "%d %d\n" h
    done
  with _ -> ()
