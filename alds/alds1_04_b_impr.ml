let bin_search x (a : int array) n =
  let l = ref 0 in
  let r = ref n in
  try
    while !l < !r do
      let m = (!l + !r) / 2 in
      if x = a.(m) then raise Exit;
      if x < a.(m) then r := m
      else l := m + 1;
    done;
    false
  with Exit -> true

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
  let n = read_int () in
  let s = read_seq () |> Array.of_list in
  let _ = read_int () in
  read_seq ()
  |> List.fold_left (fun acc e -> acc + (if bin_search e s n then 1 else 0)) 0
  |> Printf.printf "%d\n"
