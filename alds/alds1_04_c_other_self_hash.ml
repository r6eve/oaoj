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

let fold_left f init str =
  let acc = ref init in
  for i = 0 to String.length str - 1 do
    acc := f !acc str.[i];
  done;
  !acc

let () =
  let to_x = Array.make 128 (-1) in
  to_x.(Char.code 'A') <- 0;
  to_x.(Char.code 'C') <- 1;
  to_x.(Char.code 'G') <- 2;
  to_x.(Char.code 'T') <- 3;
  let set = Array.make 9999973 false in
  let hash s = fold_left (fun acc c -> 4*acc + to_x.(Char.code c)) 1 s in
  let n = read_int () in
  for _ = 0 to n - 1 do
    match read_line () |> split_on_char ' ' with
    | ["find"; s] -> print_endline (if set.(hash s) then "yes" else "no")
    | [_; s] -> set.(hash s) <- true
    | _ -> assert false
  done
