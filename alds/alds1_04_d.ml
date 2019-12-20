let split_on_char sep s =
  let open String in
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

let calc_p n k a =
  let rec bin_search l r =
    if r < l then l
    else
      let p = (l + r) / 2 in
      if count_n (0, 0, 0, p) < n then bin_search (p + 1) r
      else bin_search l (p - 1)
  and count_n = function
    | (i, j, _, _) when i = n || j = k -> i
    | (i, j, cur_p, p) ->
      if cur_p + a.(i) <= p then count_n (i + 1, j, cur_p + a.(i), p)
      else count_n (i, j + 1, 0, p) in
  bin_search 0 (100000 * 10000)

let () =
  match read_line () |> split_on_char ' ' |> List.map int_of_string with
  | [n; k] -> Array.init n (fun _ -> read_int ()) |> calc_p n k |> Printf.printf "%d\n"
  | _ -> assert false
