let m = 1039999
let t = Array.make m "\000"
let to_x = Array.make 128 0

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

let to_key s =
  let ret = ref 0 in
  let p = ref 1 in
  for i = 0 to String.length s - 1 do
    ret := !ret + !p * (to_x.(Char.code s.[i]));
    p := 5 * !p;
  done;
  !ret

let to_h1 k = k mod m
let to_h2 k = 1 + (k mod (m - 1))

let insert s =
  let k = to_key s in
  let h1 = to_h1 k in
  let h2 = to_h2 k in
  let rec doit i =
    let h = (h1 + h2*i) mod m in
    if t.(h) = s then ()
    else if t.(h) = "\000" then t.(h) <- s
    else doit (i + 1) in
  doit 0

let find s =
  let k = to_key s in
  let h1 = to_h1 k in
  let h2 = to_h2 k in
  let rec doit i =
    let h = (h1 + h2*i) mod m in
    if t.(h) = s then true
    else if t.(h) = "\000" then false
    else doit (i + 1) in
  doit 0

let () =
  to_x.(Char.code 'A') <- 1;
  to_x.(Char.code 'C') <- 2;
  to_x.(Char.code 'G') <- 3;
  to_x.(Char.code 'T') <- 4;
  let n = read_int () in
  for _ = 0 to n - 1 do
    match split_on_char ' ' (read_line ()) with
    | ["find"; s] -> print_endline (if find s then "yes" else "no")
    | [_; s] -> insert s
    | _ -> assert false
  done
