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

module SegmentTree = struct

  let default_val = ref 0

  let size = ref 0

  let make n init =
    default_val := init;
    let rec pow_of_2 i =
      if i > n then i
      else pow_of_2 (2 * i) in
    size := pow_of_2 1;
    Array.make (2 * (!size)) init

  let parent i = int_of_float ((float_of_int (i - 1)) /. 2.)

  let left i = 2*i + 1

  let right i = 2*i + 2

  let add s i x =
    let j = !size + i - 1 in
    s.(j) <- x + s.(j);
    let rec doit j =
      if j <= 0 then () else begin
        let p = parent j in
        s.(p) <- s.(left p) + s.(right p);
        doit p
      end in
    doit j

  let sum s a b =
    let rec doit k l r =
      if r < a || b < l then 0
      else if a <= l && r <= b then s.(k)
      else
        let m = (l + r) / 2 in
        doit (left k) l m + doit (right k) (m + 1) r in
    doit 0 0 (!size - 1)

end

module S = SegmentTree

let solve n q =
  let s = S.make n 0 in
  for _ = 0 to q - 1 do
    match IO.read_ns () with
    | c :: x :: y :: _ ->
      if c = 0 then S.add s x y
      else begin
        S.sum s x y |> print_int;
        print_newline ();
      end
    | _ -> assert false
  done

let () =
  match IO.read_ns () with
  | [n; q] -> solve n q
  | _ -> ()
