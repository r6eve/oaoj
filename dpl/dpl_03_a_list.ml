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
  List.iteri (fun j e -> if e = 0 then begin
    s.(j) <- 1;
    r := 1;
  end) c.(0);
  if !r = 0 then begin
    if Array.exists (fun l -> match l with
         | [] -> assert false
         | hd :: _ -> hd = 0) c
    then r := 1;
  end;
  Array.iteri (fun i l ->
    if i = 0 then ()
    else
      List.iteri (fun j e ->
        if j = 0 then
          if e = 1 then ()
          else t.(0) <- 1
        else
          if e = 1 then t.(j) <- 0
          else begin
            t.(j) <- min s.(j-1) (min s.(j) t.(j-1)) + 1;
            if t.(j) > !r then r := t.(j)
          end)
        l)
    c;
  !r * !r

let () =
  match IO.read_ns () with
  | [h; w] ->
    Array.init h (fun _ -> IO.read_ns ()) |> solve h w |> Printf.printf "%d\n"
  | _ -> assert false
