let () =
  let (w, n) = Scanf.scanf "%d %d " (fun w n -> w, n) in
  let z = Array.init w (fun i -> i + 1) in
  for _ = 0 to n - 1 do
    let (a, b) = Scanf.scanf "%d,%d " (fun a b -> a, b) in
    let (a, b) = (a - 1, b - 1) in
    let t = z.(a) in
    z.(a) <- z.(b);
    z.(b) <- t;
  done;
  Array.iter (fun e-> Printf.printf "%d\n" e) z
