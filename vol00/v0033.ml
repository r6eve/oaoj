module IO = struct

  (* @since 4.04.0 *)
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

  let read_ns () = read_line () |> split_on_char ' ' |> List.map int_of_string

end

let () =
  let rec p (x, y) = function
    | [] -> true
    | z :: _ when x > z && y > z -> false
    | z :: tl ->
      if x < z && y > z then p (z, y) tl
      else if x > z && y < z then p (x, z) tl
      else if x < z && y < z then p (x, z) tl || p (z, y) tl
      else false in
  let n = read_int () in
  for _ = 0 to n - 1 do
    let l = IO.read_ns () in
    print_endline (if p (0, 0) l then "YES" else "NO")
  done
