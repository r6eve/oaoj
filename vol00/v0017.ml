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

  let read_ss () = read_line () |> split_on_char ' '

end

let () =
  let w = Char.code 'z' - Char.code 'a' + 1 in
  let rec g l =
    if List.exists (fun e -> e = "the" || e = "this" || e = "that") l then String.concat " " l |> print_endline
    else
      List.map (fun s ->
        String.map (fun c ->
          if c < 'a' || c > 'z' then c
          else Char.code 'a' + ((Char.code c - Char.code 'a' + 1) mod w) |> Char.chr)
        s)
      l |> g in
  try
    while true do
      IO.read_ss () |> g;
    done
  with _ -> ()
