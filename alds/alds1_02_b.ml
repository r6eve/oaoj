let cnt = ref 0

let selection_sort a n =
  let swap i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in
  for i = 0 to n - 1 do
    let mini = ref i in
    for j = i to n - 1 do
      if a.(j) < a.(!mini) then mini := j;
    done;
    if !mini <> i then begin
      swap i !mini;
      incr cnt;
    end;
  done

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
  let a = read_line () |> split_on_char ' ' |> List.map int_of_string |> Array.of_list in
  selection_sort a n;
  Array.iteri (fun i n -> Printf.printf (if i = 0 then "%d" else " %d") n) a;
  print_newline ();
  Printf.printf "%d\n" !cnt
