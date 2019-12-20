module IO = struct

  let read_ss () = String.split_on_char ' ' @@ read_line ()

  let read_ns () = List.map int_of_string @@ read_ss ()

end

module SegmentTree = struct

  let default_val = ref 0

  let size = ref 0

  let pow x n =
    let rec doit x n acc =
      if n = 0 then acc
      else if n mod 2 = 0 then doit (x * x) (n / 2) acc
      else doit (x * x) (n / 2) (acc * x) in
    doit x n 1

  let make n init =
    default_val := init;
    size := pow 2 n;
    Array.make (2 * (!size) - 1) init

  let parent i = int_of_float @@ float (i - 1) /. 2.

  let left i = 2 * i + 1

  let right i = 2 * i + 2

  let update (s : int array) i x =
    let j = !size + i - 1 in
    s.(j) <- x;
    let rec doit j =
      if j <= 0 then ()
      else begin
        let p = parent j in
        let l = s.(left p) in
        let r = s.(right p) in
        s.(p) <- if l < r then l else r;
        doit p
      end in
    doit j

  let add s i x =
    let j = !size + i - 1 in
    s.(j) <- x + s.(j);
    let rec doit j =
      if j <= 0 then ()
      else begin
        let p = parent j in
        s.(p) <- s.(left p) + s.(right p);
        doit p
      end in
    doit j

  (* [a, b) *)
  let walk s a b def f =
    let rec doit k l r =
      if r <= a || b <= l then def
      else if a <= l && r <= b then s.(k)
      else
        let m = (l + r) / 2 in
        f (doit (left k) l m) (doit (right k) m r) in
    doit 0 0 !size

  let find_min s a b = walk s a b !default_val min

  let sum s a b = walk s a b 0 (+)

end

module S = SegmentTree

let solve n q =
  let s = S.make n 2147483647 in
  for _ = 0 to q - 1 do
    match IO.read_ns () with
    | c :: x :: y :: _ ->
      if c = 0 then S.update s x y
      else begin
        print_int @@ S.find_min s x (y + 1);
        print_newline ();
      end
    | _ -> assert false
  done

let () =
  match IO.read_ns () with
  | [n; q] -> solve n q
  | _ -> assert false
