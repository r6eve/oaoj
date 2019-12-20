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

let dfs vs n =
  let d = Array.make n (-1) in
  let rec doit i id =
    d.(i) <- id;
    List.iter (fun v -> if d.(v) = (-1) then doit v id) vs.(i) in
  Array.iteri (fun i e -> if e = (-1) then doit i i) d;
  d

let read_ns () =
  match read_line () |> split_on_char ' ' |> List.map int_of_string with
  | [x; y] -> (x, y)
  | _ -> assert false

let () =
  let (n, m) = read_ns () in
  let vs = Array.make n [] in
  for _ = 0 to m - 1 do
    let (s, t) = read_ns () in
    vs.(s) <- t :: vs.(s);
    vs.(t) <- s :: vs.(t);
  done;
  let d = dfs vs n in
  let q = read_int () in
  for _ = 0 to q - 1 do
    let (s, t) = read_ns () in
    Printf.printf (if d.(s) = d.(t) then "yes\n" else "no\n")
  done
