let bfs vs n =
  let d = Array.make n (-1) in
  let q = Queue.create () in
  d.(0) <- 0;
  Queue.add 0 q;
  while not (Queue.is_empty q) do
    let i = Queue.take q in
    List.iter (fun v ->
      if d.(v-1) <> (-1) then ()
      else begin
        d.(v-1) <- d.(i) + 1;
        Queue.add (v-1) q;
      end)
      vs.(i);
  done;
  d

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
  bfs vs n |> Array.iteri (fun i e -> Printf.printf "%d %d\n" (i + 1) e)
