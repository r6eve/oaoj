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

  let read_ns () = read_line () |> split_on_char ' ' |> List.map int_of_string

end

module UnionFind = struct

  type t = { mutable parent : int; mutable rank : int }

  let make n = Array.init n (fun i -> { parent = i; rank = 0 })

  let find s x =
    let rec doit x =
      let p = s.(x).parent in
      if x = p then x
      else begin
        s.(x).parent <- doit p;
        s.(x).parent
      end in
    doit x

  let unite s x y =
    let px = find s x in
    let py = find s y in
    if s.(px).rank > s.(py).rank then s.(py).parent <- px
    else begin
      s.(px).parent <- py;
      if s.(px).rank = s.(py).rank then s.(py).rank <- s.(py).rank + 1
    end

  let same_p s x y = find s x = find s y

end

module U = UnionFind

let solve n q =
  let s = U.make n in
  for _ = 0 to q - 1 do
    match IO.read_ns () with
    | n :: x :: y :: _ ->
      if n = 0 then U.unite s x y
      else print_endline (if U.same_p s x y then "1" else "0")
    | _ -> assert false
  done

let () =
  match IO.read_ns () with
  | [n; q] -> solve n q
  | _ -> assert false
