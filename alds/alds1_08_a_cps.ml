type tree = Nil | Node of int * tree * tree

let create () = Nil

let insert x t =
  let rec doit t id = match t with
    | Nil -> id (Node (x, Nil, Nil))
    | Node (y, l, r) ->
      if x < y then doit l (fun a -> id (Node (y, a, r)))
      else doit r (fun b -> id (Node (y, l, b))) in
  doit t (fun i -> i)

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
    | _ -> print !t
  done
