let memoize n func =
  let open Hashtbl in
  let m = create ~random:true n in
  let rec get x =
    try find m x with Not_found ->
      let ret = func get x in
      add m x ret;
      ret in
  get

let memoize' n func =
  let open Hashtbl in
  let m = create ~random:true n in
  let rec get x =
    match find m x with
    | y -> y
    | exception Not_found ->
      let ret = func get x in
      add m x ret;
      ret in
  get

let fib =
  fun f n ->
    if n < 2 then 1
    else f (n - 2) + f (n - 1)

let () =
  let n = read_int () in
  let fib_memo = memoize n fib in
  fib_memo n |> Printf.printf "%d\n"
