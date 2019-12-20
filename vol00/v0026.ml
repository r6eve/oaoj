let () =
  let a = Array.make 100 0 in
  let drop (x, y) =
    if x >= 0 && x <=9 && y >= 0 && y <= 9 then a.(10*y + x) <- a.(10*y + x) + 1 in
  let rec doit (x, y) = function
    | 1 -> List.iter drop [(x, y - 1); (x - 1, y); (x, y); (x + 1, y); (x, y + 1)]
    | 2 ->
      doit (x, y) 1;
      List.iter drop [(x - 1, y - 1); (x + 1, y - 1); (x - 1, y + 1); (x + 1, y + 1)]
    | 3 ->
      doit (x, y) 2;
      List.iter drop [(x, y - 2); (x - 2, y); (x + 2, y); (x, y + 2)]
    | _ -> assert false in
  try
    while true do
      let (x, y, s) = Scanf.scanf "%d,%d,%d " (fun x y s -> x, y, s) in
      doit (x, y) s
    done
  with _ ->
    Array.fold_left (fun z e -> z + if e = 0 then 1 else 0) 0 a |> Printf.printf "%d\n";
    Array.fold_left (fun z e -> max z e) 0 a |> Printf.printf "%d\n"
