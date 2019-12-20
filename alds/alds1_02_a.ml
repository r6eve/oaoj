let cnt = ref 0

let bubble_sort a n =
  let rec doit flag =
    if flag then doit (bubble (n - 1) false)
  and bubble i flag =
    if i = 0 then flag
    else if a.(i-1) <= a.(i) then bubble (i - 1) flag
    else begin
      let tmp = a.(i) in
      a.(i) <- a.(i-1);
      a.(i-1) <- tmp;
      incr cnt;
      bubble (i - 1) true
    end in
  doit true

let () =
  let n = read_int () in
  let a = read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string |> Array.of_list in
  bubble_sort a n;
  Array.iteri (fun i n -> Printf.printf (if i = 0 then "%d" else " %d") n) a;
  print_newline ();
  Printf.printf "%d\n" !cnt
