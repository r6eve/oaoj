module UnionFind = struct

  let make n = Array.init n (fun i -> i)

  let find s u =
    let rec doit u =
      if u = s.(u) then u
      else begin
        s.(u) <- doit s.(u);
        s.(u)
      end in
    doit u

  let unite s x y = s.(find s x) <- find s y

  let same_p s x y = find s x = find s y

end

module U = UnionFind

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

let solve n q =
  let s = U.make n in
  for _ = 0 to q - 1 do
    match read_ns () with
    | n :: x :: y :: _ ->
      if n = 0 then U.unite s x y
      else print_endline (if U.same_p s x y then "1" else "0")
    | _ -> assert false
  done

let () =
  match read_ns () with
  | [n; q] -> solve n q
  | _ -> assert false
