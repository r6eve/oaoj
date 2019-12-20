let () =
  let a = Array.init 10 (fun _ -> read_int ()) in
  Array.fast_sort (fun x y -> y - x) a;
  for i = 0 to 2 do
    Printf.printf"%d\n" a.(i)
  done
