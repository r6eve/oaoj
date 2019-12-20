type tree = Nil | Node of int * tree * tree

let create () = Nil

let insert x t =
  let rec doit = function
    | Nil -> Node (x, Nil, Nil)
    | Node (y, l, r) ->
      if x < y then Node (y, doit l, r)
      else Node (y, l, doit r) in
  doit t

let preorder f t =
  let rec doit = function
    | Nil -> ()
    | Node (x, l, r) -> f x; doit l; doit r in
  doit t

let inorder f t =
  let rec doit = function
    | Nil -> ()
    | Node (x, l, r) -> doit l; f x; doit r in
  doit t

let print t =
  inorder (fun e -> print_string " "; print_int e) t;
  print_newline ();
  preorder (fun e -> print_string " "; print_int e) t;
  print_newline ()

let () =
  let m = read_int () in
  let t = ref (create ()) in
  for _ = 0 to m - 1 do
    match read_line () |> split_on_char ' ' with
    | ["insert"; n] -> t := insert (int_of_string n) !t
    | _ -> print !t
  done
