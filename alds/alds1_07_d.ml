let break p lst =
  let rec doit acc = function
    | [] -> (List.rev acc, [])
    | x :: xs as l ->
      if p x then (List.rev acc, l)
      else doit (x :: acc) xs in
  doit [] lst

let split_n n lst =
  let rec doit n acc l =
    if n = 0 then (List.rev acc, l)
    else
      match l with
      | [] -> assert false
      | hd :: tl -> doit (n - 1) (hd :: acc) tl in
  doit n [] lst

let make_postorder preorder inorder =
  let rec doit = function
    | ([], []) -> []
    | ([], _) -> assert false
    | (x :: xs, ys) ->
      begin match break (fun e -> e = x) ys with
      | (_, []) -> assert false
      | (yl, _ :: yr) ->
        let (xl, xr) = split_n (List.length yl) xs in
        doit (xl, yl) @ doit (xr, yr) @ [x]
      end in
  doit (preorder, inorder)

let () =
  let n = read_int () in
  let rec read_tree i acc =
    if i = n then List.rev acc
    else read_tree (i + 1) ((Scanf.scanf "%d " (fun i -> i)) :: acc) in
  let read_tree i = read_tree i [] in
  let preorder = read_tree 0 in
  let inorder = read_tree 0 in
  make_postorder preorder inorder
  |> List.iteri (fun i e -> if i <> 0 then print_string " "; print_int e);
  print_newline ()
