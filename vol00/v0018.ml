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
  IO.read_ns ()
  |> List.fast_sort (fun x y -> y - x)
  |> List.iteri (fun i e -> Printf.printf (if i = 0 then "%d" else " %d") e);
  print_newline ()
