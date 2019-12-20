let dfs vs n =
  let d = Array.make n 0 in
  let f = Array.make n 0 in
  let rec doit i time =
    d.(i) <- time;
    let time =
      List.fold_left (fun time v ->
        if d.(v-1) <> 0 then time else doit (v-1) time)
        (time + 1) vs.(i) in
    f.(i) <- time;
    time + 1 in
  Array.fold_left (fun (i, time) e ->
    (i + 1, if e = 0 then doit i time else time))
    (0, 1) d |> ignore;
  (d, f)

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

let () =
  let n = read_int () in
  let vs = Array.make n [] in
  for _ = 0 to n - 1 do
    match read_line () |> split_on_char ' ' |> List.map int_of_string with
    | u :: _ :: l -> vs.(u-1) <- l
    | _ -> assert false
  done;
  let (d, f) = dfs vs n in
  for i = 0 to n - 1 do
    Printf.printf "%d %d %d\n" (i + 1) d.(i) f.(i)
  done
