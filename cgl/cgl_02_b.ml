module Point = struct

  type t = { x : float; y : float }

  let make x y = { x; y }

  let equal_p a b = abs_float (a.x -. b.x) < 1e-10 && abs_float (a.y -. b.y) < 1e-10

  let sum a b = { x = a.x +. b.x; y = a.y +. b.y }

  let diff a b = { x = a.x -. b.x; y = a.y -. b.y }

  let multi p k = { x = p.x *. k; y = p.y *. k }

  let div p k = { x = p.x /. k; y = p.y /. k }

  let norm p = p.x *. p.x +. p.y *. p.y

  let abs p = norm p |> sqrt

  let dot a b = a.x *. b.x +. a.y *. b.y

  let cross a b = a.x *. b.y -. a.y *. b.x

  let project p1 p2 p =
    let base = diff p2 p1 in
    dot (diff p p1) base /. norm base
    |> multi base
    |> sum p1

  let reflect p1 p2 p =
    multi (diff (project p1 p2 p) p) 2.
    |> sum p

  let parallel_p (p0, p1) (p2, p3) =
    let a = diff p0 p1 in
    let b = diff p2 p3 in
    cross a b = 0.

  let orthogonal_p (p0, p1) (p2, p3) =
    let a = diff p0 p1 in
    let b = diff p2 p3 in
    dot a b = 0.

end

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

  let read_fs () = read_line () |> split_on_char ' ' |> List.map float_of_string

end

open Point

let eps = 1e-10

let ccw p0 p1 p2 =
  let a = diff p1 p0 in
  let b = diff p2 p0 in
  let c = cross a b in
  if c > eps then 1
  else if c < (-. eps) then (-1)
  else if dot a b < (-. eps) then 2
  else if norm a < norm b then (-2)
  else 0

let intersect_p (p0, p1) (p2, p3) =
  ccw p0 p1 p2 * ccw p0 p1 p3 <= 0 && ccw p2 p3 p0 * ccw p2 p3 p1 <= 0

let () =
  let q = read_int () in
  for _ = 0 to q - 1 do
    match IO.read_fs () with
    | x0 :: y0 :: x1 :: y1 :: x2 :: y2 :: x3 :: y3 :: _ ->
      let s = (make x0 y0, make x1 y1) in
      let t = (make x2 y2, make x3 y3) in
      print_endline (if intersect_p s t then "1" else "0")
    | _ -> assert false
  done
