let to_list str =
  let rec doit i acc =
    if i < 0 then acc
    else doit (i - 1) (String.get str i :: acc) in
  doit (String.length str - 1) []

let plus s t =
  let f x = if x <= 9 then (x, 0) else (x mod 10, 1) in
  let rec doit acc z = function
    | ([], []) -> if z = 1 then 1 :: acc else acc
    | (x :: xs, []) ->
      let (s, z) = x+z |> f in
      doit (s :: acc) z (xs, [])
    | ([], y :: ys) ->
      let (s, z) = y+z |> f in
      doit (s :: acc) z ([], ys)
    | (x :: xs, y :: ys) ->
      let (s, z) = x+y+z |> f in
      doit (s :: acc) z (xs, ys) in
  let g x = to_list x |> List.map (fun c -> Char.code c - Char.code '0') in
  doit [] 0 (g s, g t)

let () =
  let n = read_int () in
  for _ = 0 to n - 1 do
    let s = read_line () in
    let t = read_line () in
    let l = String.length s in
    let m = String.length t in
    if l > 80 || m > 80 then print_endline "overflow"
    else if l < 10 && m < 10 then int_of_string s + int_of_string t |> Printf.printf "%d\n"
    else begin
      let p = plus s t in
      if List.length p > 80 then print_endline "overflow"
      else begin
        List.iter (fun e -> print_int e) p;
        print_newline ()
      end
    end
  done
