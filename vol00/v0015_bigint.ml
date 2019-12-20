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
      let x = Big_int.big_int_of_string s in
      let y = Big_int.big_int_of_string t in
      let p = Big_int.add_big_int x y |> Big_int.string_of_big_int in
      if String.length p > 80 then print_endline "overflow"
      else print_endline p
    end
  done
