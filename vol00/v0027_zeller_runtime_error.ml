let () =
  let weekdays = [|"Saturday"; "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"|] in
  let zeller year month day =
    let q = day in
    let (y, m) =
      if month = 1 || month = 2 then (year - 1, month + 12)
      else (year, month) in
    let h = float year /. 100. in
    let y = year mod 100 in
    weekdays.(
      (
      y
      + (float y /. 4. |> floor |> truncate)
      + (h /. 4. |> floor |> truncate)
      - (2. *. h |> floor |> truncate)
      + (float (13 * (m + 1)) /. 5. |> floor |> truncate)
      + q
      ) mod 7) in
  let rec main_loop () =
    let (m, d) = Scanf.scanf "%d %d " (fun m d -> m, d) in
    if m = 0 && d = 0 then ()
    else begin
      zeller 2014 m d |> print_endline;
      main_loop ()
    end in
  main_loop ()
