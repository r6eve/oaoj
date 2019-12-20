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

let gcd m n =
  let rec doit x y =
    if y <= 0 then x else doit y (x mod y) in
  doit (max m n) (min m n)

let lcm m n = m / (gcd m n) * n

let () =
  let _ = read_int () in
  match IO.read_ns () with
  | x :: xs -> List.fold_left lcm x xs |> Printf.printf "%d\n"
  | _ -> assert false
