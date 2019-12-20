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

type t = { h : int; i : int }

let max (x : int) y = if x > y then x else y

let solve ans w v =
  let (m, _, xs) = Array.fold_left (fun (m, i, xs) h -> match xs with
    | [] -> (m, i + 1, [ { h; i } ])
    | x :: _ ->
      if x.h = h then (m, i + 1, xs)
      else if x.h < h then (m, i + 1, { h; i } :: xs)
      else
        let rec doit m j = function
          | [] -> (m, i + 1, [ { h; i = j } ])
          | x :: tl as xs ->
            if x.h >= h then doit (max (x.h * (i - x.i)) m) x.i tl
            else (m, i + 1, { h; i = j } :: xs) in
        doit m i xs)
    (ans, 0, [])
    v in
  List.fold_left (fun m x -> max (x.h * (w - x.i)) m) m xs

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
