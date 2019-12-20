let split_on_char sep s =
  let open String in
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

let () =
  match
    read_line () |> split_on_char ' ' |> List.fold_left (fun acc n ->
      if n <> "+" && n <> "-" && n <> "*" then int_of_string n :: acc
      else
        match acc with
        | x :: y :: acc ->
          if n = "+" then (y + x) :: acc
          else if n = "-" then (y - x) :: acc
          else (y * x) :: acc
        | _ -> assert false) []
  with
  | ans :: _ -> Printf.printf "%d\n" ans
  | _ -> assert false
