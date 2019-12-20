module Geometry = struct

  type t = { x : float; y : float }

  let make x y = { x; y }

  let eps = 1e-10

  let equal_p a b = abs_float (a.x -. b.x) < eps && abs_float (a.y -. b.y) < eps

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

  let orthogonal_p (p0, p1) (p2, p3) = dot (diff p0 p1) (diff p2 p3) |> abs_float < eps

  let parallel_p (p0, p1) (p2, p3) = cross (diff p0 p1) (diff p2 p3) |> abs_float < eps

  let ccw p0 p1 p2 =
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

  let distance s t =
    if intersect_p s t then 0.
    else
      let p2p a b = diff a b |> abs in
      let l2p (p0, p1) p =
        cross (diff p1 p0) (diff p p0) /. p2p p1 p0 |> abs_float in
      let s2p s p =
        let (p0, p1) = s in
        if dot (diff p1 p0) (diff p p0) < 0. then p2p p p0
        else if dot (diff p0 p1) (diff p p1) < 0. then p2p p p1
        else l2p s p in
      let (p0, p1) = s in
      let (p2, p3) = t in
      min (min (s2p s p2) (s2p s p3)) (min (s2p t p0) (s2p t p1))

  let convex_p p0 p1 p2 p3 =
    let c = ccw p0 p1 p2 in
    List.for_all (fun e -> c = e) [ccw p1 p2 p3; ccw p2 p3 p0; ccw p3 p0 p1]

end

module G = Geometry

let () =
  try
    while true do
      let (xa, ya, xb, yb, xc, yc, xd, yd) =
        Scanf.scanf "%f,%f,%f,%f,%f,%f,%f,%f " (fun xa ya xb yb xc yc xd yd ->
          xa, ya, xb, yb, xc, yc, xd, yd) in
      print_endline
        (if G.convex_p (G.make xa ya) (G.make xb yb) (G.make xc yc) (G.make xd yd)
         then "YES"
         else "NO")
    done
  with _ -> ()
