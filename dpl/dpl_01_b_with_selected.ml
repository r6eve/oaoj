type t = { v : int; w : int }

let solve n w a =
  let s = Array.make (w + 1) 0 in
  let t = Array.copy s in
  let g = Array.make (w + 1) { v = 0; w = 0 } in
  for i = 0 to n - 1 do
    for j = a.(i).w to w do
      if s.(j - a.(i).w) + a.(i).v > s.(j) then begin
        t.(j) <- s.(j - a.(i).w) + a.(i).v;
        g.(j) <- a.(i);
      end
    done;
    Array.iteri (fun i e -> s.(i) <- e) t;
  done;
  let rec doit i =
    if i = 0 then ()
    else begin
      Printf.printf "%d %d\n" g.(i).v g.(i).w;
      doit (i - g.(i).w)
    end in
  doit w;
  t.(w)

let () =
  let (n, w) = Scanf.scanf "%d %d " (fun n w -> n, w) in
  Array.init n (fun _ -> Scanf.scanf "%d %d " (fun v w -> { v; w }))
  |> solve n w |> Printf.printf "%d\n"
