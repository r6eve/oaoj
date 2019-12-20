let () =
  let n = read_int () in
  for _ = 0 to n - 1 do
    match Scanf.scanf "%d %d %d " (fun x y z -> [x; y; z]) |> List.sort (-) with
    | [x; y; z] -> print_endline (if z*z = x*x + y*y then "YES" else "NO")
    | _ -> ()
  done
