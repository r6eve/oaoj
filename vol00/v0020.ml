let () =
  let w = Char.code 'a' - Char.code 'A' in
  read_line ()
  |> String.map (fun c -> if c < 'a' || c > 'z' then c else Char.code c - w |> Char.chr)
  |> print_endline
