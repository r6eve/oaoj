let () =
  let rec main_loop () =
    let n = read_int () in
    if n = 0 then ()
    else begin
      let rec doit i ans sum =
        if i = n then ans
        else
          let a = read_int () in
          let sum = max a (sum + a) in
          doit (i + 1) (max ans sum) sum in
      doit 0 min_int 0 |> Printf.printf "%d\n";
      main_loop ()
    end in
  main_loop ()
