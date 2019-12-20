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
    dot (diff p p1) base /. norm base |> multi base |> sum p1

  let reflect p1 p2 p = multi (diff (project p1 p2 p) p) 2. |> sum p

  let orthogonal_p (p0, p1) (p2, p3) = dot (diff p0 p1) (diff p2 p3) = 0.

  let parallel_p (p0, p1) (p2, p3) = cross (diff p0 p1) (diff p2 p3) = 0.

  let ccw p0 p1 p2 =
    let eps = 1e-10 in
    let a = diff p1 p0 in
    let b = diff p2 p0 in
    let c = cross a b in
    if c > eps then 1 (* COUNTER_CLOCKWISE *)
    else if c < (-. eps) then (-1) (* CLOCKWISE *)
    else if dot a b < (-. eps) then 2 (* ONLINE_BACK *)
    else if norm a < norm b then (-2) (* ONLINE_FRONT *)
    else 0 (* ON_SEGMENT *)

  let intersect_p (p0, p1) (p2, p3) =
    ccw p0 p1 p2 * ccw p0 p1 p3 <= 0 && ccw p2 p3 p0 * ccw p2 p3 p1 <= 0

  let cross_point (p0, p1) (p2, p3) =
    let base = diff p3 p2 in
    let d1 = diff p0 p2 |> cross base |> abs_float in
    let d2 = diff p1 p2 |> cross base |> abs_float in
    d1 /. (d1 +. d2) |> multi (diff p1 p0) |> sum p0

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

let () =
  let q = read_int () in
  for _ = 0 to q - 1 do
    match IO.read_fs () with
    | x0 :: y0 :: x1 :: y1 :: x2 :: y2 :: x3 :: y3 :: _ ->
      let s = (make x0 y0, make x1 y1) in
      let t = (make x2 y2, make x3 y3) in
      let p = cross_point s t in
      Printf.printf "%.10f %.10f\n" p.x p.y
    | _ -> assert false
  done
