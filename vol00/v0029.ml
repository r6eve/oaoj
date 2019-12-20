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
  let l = IO.read_ss () |> List.fast_sort compare in
  let f =
    List.fold_left (fun acc s -> match acc with
      | [] -> [(0, s)]
      | (n, w) :: tl when w = s -> (n + 1, w) :: tl
      | acc -> (0, s) :: acc)
      [] l
    |> List.fast_sort (fun (a, _) (b, _) -> b - a)
    |> List.hd |> snd in
  List.sort (fun a b -> String.length b - String.length a) l
  |> List.hd |> Printf.printf "%s %s\n" f;
