let quick_sort (a : (int * char * int) array) n cmp =
  let swap i j = let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp in
  let partition p r =
    let i = ref p in
    for j = p to r - 1 do
      if cmp a.(j) a.(r) <= 0 then begin
        swap !i j;
        incr i;
      end
    done;
    swap !i r;
    !i in
  let rec doit p r =
    if p >= r then () else
    let q = partition p r in
    doit p (q - 1);
    doit (q + 1) r in
  doit 0 (n - 1)

let stable_p (a : (int * char * int) array) n =
  try
    for i = 1 to n - 1 do
      if (fun (xi, _, xd) (yi, _, yd) -> xd = yd && xi > yi) a.(i-1) a.(i) then raise Exit
    done;
    true
  with Exit -> false

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

let () =
  let n = read_int () in
  let a = Array.init n (fun i ->
    match read_line () |> split_on_char ' ' with
    | [c; d] -> (i, c.[0], int_of_string d)
    | _ -> assert false) in
  quick_sort a n (fun (_, _, x) (_, _, y) -> compare x y);
  print_endline (if stable_p a n then "Stable" else "Not stable");
  Array.iter (fun (_, c, d) -> Printf.printf "%c %d\n" c d) a
