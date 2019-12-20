let counting_sort a b k n =
  let c = Array.make (k + 1) 0 in
  Array.iter (fun e -> c.(e) <- c.(e) + 1) a;
  for i = 1 to k do
    c.(i) <- c.(i) + c.(i-1)
  done;
  for i = n - 1 downto 0 do
    c.(a.(i)) <- c.(a.(i)) - 1;
    b.(c.(a.(i))) <- a.(i);
  done

let () =
  let n = read_int () in
  let a = Array.init n (fun _ -> Scanf.scanf "%d " (fun e -> e)) in
  let b = Array.make n 0 in
  counting_sort a b 10000 n;
  print_int b.(0);
  for i = 1 to n - 1 do
    print_string " "; print_int b.(i);
  done;
  print_newline ()
