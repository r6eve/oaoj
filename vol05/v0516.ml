let solve n k =
  let a = Array.init n (fun _ -> Scanf.scanf "%d " (fun i -> i)) in
  let s = Array.make (n + 1) 0 in
  for i = 1 to n - 1 do
    s.(i + 1) <- s.(i) + a.(i);
  done;
  let ret = ref min_int in
  for i = 0 to n - k do
    ret := max !ret (s.(k + i) - s.(i));
  done;
  !ret

let () =
  let rec doit () =
    let (n, k) = Scanf.scanf "%d %d " (fun n k -> n, k) in
    if n = 0 && k = 0 then ()
    else begin
      Printf.printf "%d\n" @@ solve n k;
      doit ()
    end in
  doit ()
