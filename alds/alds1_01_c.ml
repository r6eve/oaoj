let pow x n m =
  let rec doit x n acc =
    if n = 0 then acc
    else if n mod 2 = 0 then doit (x * x mod m) (n / 2) acc
    else doit (x * x mod m) (n / 2) (acc * x mod m) in
  doit x n 1

(*
let prime_p n =
  if n <= 2 then true
  else if n mod 2 = 0 then false
  else if pow 2 (n - 1) n = 1 then true
  else false
*)

let prime_p n =
  n <= 2 || (n mod 2 <> 0 && pow 2 (n - 1) n = 1)

let () =
  let n = read_int () in
  let rec doit i acc =
    if i = n then acc
    else doit (i + 1) (acc + (if read_int () |> prime_p then 1 else 0)) in
  doit 0 0 |> Printf.printf "%d\n"
