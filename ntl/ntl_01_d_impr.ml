let prime_factors n =
  let rec doit i x acc =
    if i*i > n then
      if x = 1 then acc else x :: acc
    else begin
      if x mod i = 0 then begin
        let new_x = ref (x / i) in
        while !new_x mod i = 0 do
          new_x := !new_x / i
        done;
        doit (i + 1) !new_x (i :: acc)
      end else
        doit (i + 1) x acc
    end in
  doit 2 n []

let phi_func n =
  prime_factors n
  |> List.fold_left (fun acc x -> acc *. (1. -. 1. /. (float x))) (float n)
  |> int_of_float

let () =
  let n = read_int () in
  phi_func n |> Printf.printf "%d\n"
