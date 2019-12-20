let iota ?(start=0) ?(step=1) cnt =
  let rec doit i acc =
    if i <= 0 then acc
    else doit (i - 1) (start + step*(i - 1) :: acc) in
  doit cnt []

let rec solve (n, s) =
  let rec doit n s lst =
    match (n, s) with
    | (0, 0) -> 1
    | (0, _) -> 0
    | (n, s) ->
      List.map (fun e ->
        List.filter (fun x -> x > e) lst |> doit (n - 1) (s - e))
        lst
      |> List.fold_left (+) 0 in
  iota 10 |> doit n s

let () =
  let rec main_loop () =
    let (n, s) = Scanf.scanf "%d %d " (fun n s -> n, s) in
    if n = 0 && s = 0 then ()
    else begin
      solve (n, s) |> Printf.printf "%d\n";
      main_loop ()
    end in
  main_loop ()
