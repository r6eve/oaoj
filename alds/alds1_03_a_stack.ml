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
  let open Stack in
  let s = create () in
  read_line () |> split_on_char ' ' |> List.iter (fun n ->
    if n <> "+" && n <> "-" && n <> "*" then push (int_of_string n) s
    else
      let x = pop s in
      let y = pop s in
      if n = "+" then push (y + x) s
      else if n = "-" then push (y - x) s
      else push (y * x) s);
  pop s |> Printf.printf "%d\n"
