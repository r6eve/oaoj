let make_fib_memo () =
  let m = Array.make 45 0 in
  m.(0) <- 1;
  m.(1) <- 1;
  fun n ->
    let rec doit i =
      if m.(i) <> 0 then m.(i)
      else begin
        m.(i) <- doit (i - 2) + doit (i - 1);
        m.(i);
      end in
    doit n

let () =
  let fib_memo = make_fib_memo () in
  read_int () |> fib_memo |> Printf.printf "%d\n"
