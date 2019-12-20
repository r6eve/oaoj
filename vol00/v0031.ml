let solve w =
  let rec doit i s acc =
    let g = 2. ** float i |> truncate in
    if g = s then g :: acc
    else if g > s then doit (i - 1) s acc
    else doit (i - 1) (s - g) (g :: acc) in
  doit 9 w []

let () =
  try
    while true do
      read_int ()
      |> solve
      |> List.iteri (fun i e -> Printf.printf (if i = 0 then "%d" else " %d") e);
      print_newline ()
    done
  with _ -> ()
