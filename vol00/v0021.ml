module Geometry = struct

  type t = { x : float; y : float }

  let eps = 1e-10

  let make x y = { x; y }

  let diff a b = { x = a.x -. b.x; y = a.y -. b.y }

  let cross a b = a.x *. b.y -. a.y *. b.x

  let parallel_p (p0, p1) (p2, p3) = abs_float (cross (diff p0 p1) (diff p2 p3)) < eps

end

module G = Geometry

let () =
  let n = read_int () in
  for _ = 0 to n - 1 do
    let (x1, y1, x2, y2, x3, y3, x4, y4) =
      Scanf.scanf "%f %f %f %f %f %f %f %f " (fun x1 y1 x2 y2 x3 y3 x4 y4 ->
        x1, y1, x2, y2, x3, y3, x4, y4) in
    print_endline
      (if G.parallel_p (G.make x1 y1, G.make x2 y2) (G.make x3 y3, G.make x4 y4)
       then "YES" else "NO")
  done
