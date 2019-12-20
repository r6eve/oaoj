let () =
  let days = [|0; 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30|] in
  let weekdays = [|"Wednesday"; "Thursday"; "Friday"; "Saturday"; "Sunday"; "Monday"; "Tuesday"|] in
  let solve month day =
    let rec doit i sum =
      if i = month then sum
      else doit (i + 1) (sum + days.(i)) in
    weekdays.((day + doit 1 0) mod 7) in
  let rec main_loop () =
    let (m, d) = Scanf.scanf "%d %d " (fun m d -> m, d) in
    if m = 0 && d = 0 then ()
    else begin
      solve m d |> print_endline;
      main_loop ()
    end in
  main_loop ()
