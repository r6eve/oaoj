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
  try
    while true do
      let s = IO.read_ns () in
      let t = IO.read_ns () in
      let h = List.fold_left2 (fun acc a b -> acc + if a = b then 1 else 0) 0 s t in
      List.fold_left (fun b a -> b + if List.mem a t then 1 else 0) 0 s - h
      |> Printf.printf "%d %d\n" h
    done
  with _ -> ()
