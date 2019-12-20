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
  Printf.printf "\n";
  preorder (fun e -> print_string " "; print_int e) t;
  Printf.printf "\n"

let find x t =
  let rec doit = function
    | Nil -> false
    | Node (y, _, _) when x = y -> true
    | Node (y, l, _) when x < y -> doit l
    | Node (_, _, r) -> doit r in
  doit t

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
  let m = read_int () in
  let t = ref (create ()) in
  for _ = 0 to m - 1 do
    match read_line () |> split_on_char ' ' with
    | ["insert"; n] -> t := insert (int_of_string n) !t
    | ["find"; n] -> print_endline (if find (int_of_string n) !t then "yes" else "no")
    | _ -> print !t
  done
