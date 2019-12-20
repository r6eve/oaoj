module SS = Set.Make(String)

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
  let rec doit set i =
    if i = n then () else
    match read_line () |> split_on_char ' ' with
    | ["find"; s] ->
      begin
        print_endline (if SS.mem s set then "yes" else "no");
        doit set (i + 1)
      end
    | [_; s] -> doit (SS.add s set) (i + 1)
    | _ -> assert false in
  doit SS.empty 0
