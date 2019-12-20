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

let solve ans w v =
  let (ans, _, xs) = Array.fold_left (fun (ans, j, xs) c -> match xs with
    | [] -> (ans, j + 1, [(c, j)])
    | (ch, _) :: _ ->
      if ch = c then (ans, j + 1, xs)
      else if ch < c then (ans, j + 1, (c, j) :: xs)
      else
        let rec f ans k = function
          | [] -> (ans, j + 1, [(c, k)])
          | (ch, cj) :: tl as ys ->
            if ch >= c then f (if ch * (j - cj) > ans then ch * (j - cj) else ans) cj tl
            else (ans, j + 1, (c, k) :: ys) in
        f ans j xs)
    (ans, 0, [])
    v in
  List.fold_left (fun acc (ch, cj) -> if ch * (w - cj) > acc then ch * (w - cj) else acc) ans xs

let () =
  match IO.read_ns () with
  | [h; w] ->
    let s = Array.make w 0 in
    let t = Array.make w 0 in
    let rec doit i ans =
      if i = h then ans
      else begin
        IO.read_ns () |> List.iteri (fun j e -> t.(j) <- if e = 1 then 0 else s.(j) + 1);
        Array.iteri (fun i e -> s.(i) <- e) t;
        solve ans w t |> doit (i + 1)
      end in
    doit 0 0 |> Printf.printf "%d\n"
  | _ -> assert false
