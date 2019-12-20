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
  let read_seq () = read_line () |> split_on_char ' ' |> List.map int_of_string in
  let _ = read_int () in
  let s = read_seq () in
  let _ = read_int () in
  let t = read_seq () in
  List.fold_left (fun acc e -> acc + (if List.mem e s then 1 else 0)) 0 t
  |> Printf.printf "%d\n"
