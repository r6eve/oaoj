let () =
  let rec doit l =
    let n = read_int () in
    if n <> 0 then doit (n :: l)
    else
      match l with
      | [] -> assert false
      | hd :: tl ->
        Printf.printf "%d\n" hd;
        doit tl in
  try doit [] with _->()
