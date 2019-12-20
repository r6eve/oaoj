let bin_search x (a : int array) n =
  let rec doit l r =
    if l >= r then false
    else
      let m = (l + r) / 2 in
      if x = a.(m) then true
      else if x < a.(m) then doit l m
      else doit (m + 1) r in
  doit 0 n

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
  let t = read_seq () in
  List.fold_left (fun acc e -> acc + (if bin_search e s n then 1 else 0)) 0 t
  |> Printf.printf "%d\n"
