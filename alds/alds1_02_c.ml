let print_array a =
  Array.iteri (fun i s -> Printf.printf (if i = 0 then "%s" else " %s") s) a;
  print_newline ()

let swap i j a = let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp

let bubble_sort a n cmp =
  let rec doit flag =
    if flag then doit (bubble (n - 1) false)
  and bubble i flag =
    if i = 0 then flag
    else if cmp a.(i-1) a.(i) <= 0 then bubble (i - 1) flag
    else begin
      swap i (i - 1) a;
      bubble (i - 1) true
    end in
  doit true

let selection_sort a n cmp =
  let swap i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in
  for i = 0 to n - 1 do
    let mini = ref i in
    for j = i to n - 1 do
      if cmp a.(j) a.(!mini) < 0 then mini := j;
    done;
    if !mini <> i then swap i !mini;
  done

let cmp_card x y =
  let num_of_char c = Char.code c - Char.code '0' in
  compare (num_of_char x.[1]) (num_of_char y.[1])

let stable_p a b n =
  let rec doit i =
    if i = n then true
    else if a.(i) <> b.(i) then false
    else doit (i + 1) in
  doit 0

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
  let a = read_line () |> split_on_char ' ' |> Array.of_list in
  let b = Array.copy a in
  bubble_sort a n cmp_card;
  selection_sort b n cmp_card;
  print_array a;
  print_endline "Stable";
  print_array b;
  print_endline (if stable_p a b n then "Stable" else "Not stable")
