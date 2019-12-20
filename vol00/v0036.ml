let to_list str =
  let rec doit i acc =
    if i < 0 then acc
    else doit (i - 1) (String.get str i :: acc) in
  doit (String.length str - 1) []

let (height, width) = (8, 8)

let read_ns () = read_line () |> to_list |> List.map (fun e -> Char.code e - Char.code '0')

let check t i j =
  let a i j = i+1 < height && j+1 < width && List.for_all ((=) 1) [t.(i).(j); t.(i).(j+1); t.(i+1).(j); t.(i+1).(j+1)] in
  let b i j = i+3 < height && List.for_all ((=) 1) [t.(i).(j); t.(i+1).(j); t.(i+2).(j); t.(i+3).(j)] in
  let c i j = j+3 < width && List.for_all ((=) 1) [t.(i).(j); t.(i).(j+1); t.(i).(j+2); t.(i).(j+3)] in
  let d i j = i+2 < height && j-1 >= 0 && List.for_all ((=) 1) [t.(i).(j); t.(i+1).(j-1); t.(i+1).(j); t.(i+2).(j-1)] in
  let e i j = i+1 < height && j+2 < width && List.for_all ((=) 1) [t.(i).(j); t.(i).(j+1); t.(i+1).(j+1); t.(i+1).(j+2)] in
  let f i j = i+2 < height && j+1 < width && List.for_all ((=) 1) [t.(i).(j); t.(i+1).(j); t.(i+1).(j+1); t.(i+2).(j+1)] in
  let g i j = i+1 < height && j-1 >= 0 && j+1 < width && List.for_all ((=) 1) [t.(i).(j); t.(i).(j+1); t.(i+1).(j-1); t.(i+1).(j)] in
  let rec doit = function
    | [] -> assert false
    | (f, s) :: tl ->
      if f i j then s
      else doit tl in
  doit [(a, "A"); (b, "B"); (c, "C"); (d, "D"); (e, "E"); (f, "F"); (g, "G")]

let () =
  try
    while true do
      let t = Array.make_matrix height width 0 in
      for i = 0 to height - 1 do
        let rec doit j = function
          | [] -> ()
          | hd :: tl ->
            t.(i).(j) <- hd;
            doit (j + 1) tl in
        read_ns () |> doit 0;
      done;
      let rec doit = function
        | (i, _) when i = height -> assert false
        | (i, j) when j = width -> doit (i + 1, 0)
        | (i, j) ->
          if t.(i).(j) = 1 then (check t i j |> print_endline)
          else doit (i, j + 1) in
      doit (0, 0);
      read_line () |> ignore;
    done
  with _ -> ()
