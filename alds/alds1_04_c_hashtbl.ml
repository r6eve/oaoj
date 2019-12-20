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
  let n = read_int () in
  let tbl = Hashtbl.create ~random:true n in
  for _ = 0 to n - 1 do
    match read_line () |> split_on_char ' ' with
    | ["find"; s] -> print_endline (if Hashtbl.mem tbl s then "yes" else "no")
    | [_; s] -> Hashtbl.add tbl s ()
    | _ -> assert false
  done
