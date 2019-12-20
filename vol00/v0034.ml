let () =
  try
    while true do
      let l = Array.init 10 (fun _ -> Scanf.scanf "%d," (fun i -> 2000*i)) in
      let (v1, v2) = Scanf.scanf"%d,%d "(fun v1 v2 -> v1, v2) in
      let x = v1 * (Array.fold_left (+) 0 l / (v1 + v2)) in
      let rec doit i z =
        let z = z + l.(i) in
        if x <= z then i + 1
        else doit (i + 1) z in
      doit 0 0 |> Printf.printf "%d\n"
    done
  with _ -> ()
