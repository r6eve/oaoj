let partition (a : int array) p r =
  let i = ref p in
  for j = p to r - 1 do
    if a.(j) <= a.(r) then begin
      let tmp = a.(!i) in
      a.(!i) <- a.(j);
      a.(j) <- tmp;
      incr i;
    end
  done;
  let tmp = a.(!i) in
  a.(!i) <- a.(r);
  a.(r) <- tmp;
  !i

let () =
  let n = read_int () in
  let a = Array.init n (fun _ -> Scanf.scanf "%d " (fun i -> i)) in
  let k = partition a 0 (n - 1) in
  print_int a.(0);
  for i = 1 to n - 1 do
    if i = k then (print_string " ["; print_int a.(i); print_string "]")
    else (print_string " "; print_int a.(i))
  done;
  print_newline ()
