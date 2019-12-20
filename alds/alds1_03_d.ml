let to_list str =
  let rec doit i acc =
    if i < 0 then acc
    else doit (i - 1) (str.[i] :: acc) in
  doit (String.length str - 1) []

let calc_area lst =
  List.fold_left2
    (fun (is, acc) i x -> match x with
      | '_' -> (is, acc)
      | '\\' -> (i :: is, acc)
      | '/' ->
        begin match is with
        | [] -> ([], acc)
        | j :: tl -> (tl, ((j, i), i - j) :: acc)
        end
      | _ -> assert false)
    ([], [])
    (List.mapi (fun i _ -> i) lst)
    lst
  |> snd

let integrale lst =
  let rec doit acc = function
    | [] -> acc
    | [e] -> e :: acc
    | ((j1, i1), a1) as hd :: (((j2, i2), a2) :: l as tl) ->
      if j1 < j2 && i2 < i1 then doit acc (((j1, i1), a1 + a2) :: l)
      else doit (hd :: acc) tl in
  doit [] lst

let () =
  let areas = read_line () |> to_list |> calc_area |> integrale in
  List.fold_left (fun acc (_, n) -> acc + n) 0 areas |> Printf.printf "%d\n";
  print_int (List.length areas);
  List.iter (fun (_, n) -> print_string " "; print_int n) areas;
  print_newline ()
