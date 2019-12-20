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

let min (x : int) y = if x < y then x else y

let solve h w c =
  let s = Array.make w 0 in
  let t = Array.make w 0 in
  let r = ref 0 in
  for j = 0 to w - 1 do
    if c.(0).(j) then begin
      s.(j) <- 1;
      r := 1;
    end
  done;
  if !r = 0 then begin
    let rec doit i =
      if i = h then ()
      else if c.(i).(0) then r := 1
      else doit (i + 1) in
    doit 0
  end;
  for i = 1 to h - 1 do
    if c.(i).(0) then t.(0) <- 1;
    for j = 1 to w - 1 do
      if not c.(i).(j) then t.(j) <- 0
      else begin
        t.(j) <- min s.(j-1) (min s.(j) t.(j-1)) + 1;
        if t.(j) > !r then r := t.(j)
      end
    done;
    Array.iteri (fun i e -> s.(i) <- e) t
  done;
  !r * !r

let () =
  match IO.read_ns () with
  | [h; w] ->
    let c = Array.make_matrix h w false in
    for i = 0 to h - 1 do
      IO.read_ns () |> List.iteri (fun j e -> c.(i).(j) <- e = 0);
    done;
    solve h w c |> Printf.printf "%d\n"
  | _ -> assert false
