let () =
  let l = read_line () in
  let n = String.length l in
  String.iteri (fun i _ -> print_char l.[n - i - 1]) l;
  print_newline ()
