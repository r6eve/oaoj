let print_array a =
  Array.iteri (fun i n -> Printf.printf (if i = 0 then "%d" else " %d") n) a;
  print_newline ()

let insertion_sort a n =
  print_array a;
  for i = 1 to n - 1 do
    let key = a.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && a.(!j) > key do
      a.(!j+1) <- a.(!j);
      decr j;
    done;
    a.(!j+1) <- key;
    print_array a;
  done

let () =
  let n = read_int () in
  let a = read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string |> Array.of_list in
  insertion_sort a n
