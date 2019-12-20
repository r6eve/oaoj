let () =
  try
    while true do
      let (x, y) = Scanf.scanf "%d %d " (fun x y -> x, y) in
      x + y |> string_of_int |> Bytes.length |> Printf.printf "%d\n"
    done
  with _ -> ()
