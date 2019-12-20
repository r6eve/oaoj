let () =
  let m = 999999 in
  let a = Array.make (m + 1) true in
  a.(0) <- false; a.(1) <- false;
  let dp = Array.make (m + 1) 0 in
  for i = 2 to m do
    if a.(i) then begin
      let n = m / i in
      for j = 2 to n do
        a.(i*j) <- false
      done
    end;
    dp.(i) <- dp.(i-1) + if a.(i) then 1 else 0;
  done;
  try
    while true do
      Printf.printf "%d\n" dp.(read_int ())
    done
  with _ -> ()
