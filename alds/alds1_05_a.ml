let max_m = 2000

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

let make_dp a n =
  let dp = Array.make_matrix (n + 1) (max_m + 1) false in
  for i = 1 to n do dp.(i).(a.(i-1)) <- true done;
  for i = 2 to n do
    for j = 1 to max_m do
      if dp.(i-1).(j) || j > a.(i-1) && dp.(i-1).(j - a.(i-1)) then dp.(i).(j) <- true
    done;
  done;
  dp

let () =
  let read_ns () = read_line () |> split_on_char ' ' |> List.map int_of_string in
  let n = read_int () in
  let a = read_ns () |> Array.of_list in
  let _ = read_int () in
  let dp = make_dp a n in
  read_ns () |> List.iter (fun e -> print_endline (if dp.(n).(e) then "yes" else "no"))
