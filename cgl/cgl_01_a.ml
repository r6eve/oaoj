module Point = struct

  type t = { x : float; y : float }

  let make x y = { x; y }

  let sum a b = { x = a.x +. b.x; y = a.y +. b.y }

  let diff a b = { x = a.x -. b.x; y = a.y -. b.y }

  let multi p k = { x = p.x *. k; y = p.y *. k }

  let norm p = p.x *. p.x +. p.y *. p.y

  let dot a b = a.x *. b.x +. a.y *. b.y

  let project p1 p2 p =
    let base = diff p2 p1 in
    dot (diff p p1) base /. norm base
    |> multi base
    |> sum p1

end

open Point

let () =
  let x1, y1, x2, y2 = Scanf.scanf "%f %f %f %f " (fun a b c d -> a,b,c,d) in
  let p1 = make x1 y1 in
  let p2 = make x2 y2 in
  let q = Scanf.scanf "%d " (fun i -> i) in
  for _ = 0 to q - 1 do
    let x, y = Scanf.scanf "%f %f " (fun x y -> x,y) in
    let p = make x y |> project p1 p2 in
    Printf.printf "%.10f %.10f\n" p.x p.y
  done
