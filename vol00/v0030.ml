let rec solve (n, s) =
  let rec doit m = function
    | (0, 0) -> 1
    | (0, _) -> 0
    | (n, s) ->
      let rec aux e acc =
        if e > s || e > 9 then acc
        else acc + doit (e + 1) (n - 1, s - e) |> aux (e + 1) in
      aux m 0 in
  doit 0 (n, s)

let () =
  let rec main_loop () =
    let (n, s) = Scanf.scanf "%d %d " (fun n s -> n, s) in
    if n = 0 && s = 0 then ()
    else begin
      solve (n, s) |> Printf.printf "%d\n";
      main_loop ()
    end in
  main_loop ()
