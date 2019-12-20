(* let (n1, n2, n_limit) = (3, 9, 31) *)
let (n1, n2, n_limit) = (4, 16, 45)

let goal = Array.init n2 (fun i -> (i + 1) mod n2)

let area = Array.make n2 []

let md = Array.make_matrix n2 n2 0

let initialize_area () =
  for i = 0 to n2 - 1 do
    let lst = [] in
    let lst = if i + n1 < n2 then (i + n1) :: lst else lst in
    let lst = if (i + 1) mod n1 <> 0 then (i + 1) :: lst else lst in
    let lst = if i - 1 >= 0 && (i - 1) mod n1 <> n1 - 1 then (i - 1) :: lst else lst in
    let lst = if i - n1 >= 0 then (i - n1) :: lst else lst in
    area.(i) <- lst;
  done

let initialize_md () =
  for i = 0 to n2 - 2 do
    for j = 0 to n2 - 1 do
      md.(i+1).(j) <- abs (i / n1 - j / n1) + abs (i mod n1 - j mod n1)
    done
  done

let idastar a limit space lower =
  let rec doit i space moved lower =
    if i = limit then
      if a <> goal then ()
      else begin
        List.length moved |> Printf.printf "%d\n";
        exit 0;
      end
    else
      List.iter (fun j ->
        let x = a.(j) in
        match moved with
        | y :: _ when x = y -> ()
        | moved ->
          let lower = lower - md.(x).(j) + md.(x).(space) in
          if lower + i > limit then ()
          else begin
            a.(j) <- 0;
            a.(space) <- x;
            doit (i + 1) j (x :: moved) lower;
            a.(j) <- x;
            a.(space) <- 0;
          end)
      area.(space) in
  doit 0 space [] lower

let findi a x =
  let n = Array.length a in
  let rec doit i =
    if i = n then assert false
    else if a.(i) = x then i
    else doit (i + 1) in
  doit 0

let () =
  initialize_area ();
  initialize_md ();
  let a = Array.init n2 (fun _ -> Scanf.scanf "%d " (fun i -> i)) in
  let space = findi a 0 in
  let lower =
    Array.fold_left (fun (i, sum) e -> (i + 1, sum + md.(e).(i))) (0, 0) a |> snd in
  for limit = lower to n_limit do
    idastar a limit space lower
  done
