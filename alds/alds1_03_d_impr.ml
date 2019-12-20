let () =
  let s1 = Stack.create () in
  let s2 = Stack.create () in
  let sum = ref 0 in
  read_line () |> String.iteri (fun i c ->
    match c with
    | '_' -> ()
    | '\\' -> Stack.push i s1
    | '/' ->
      if Stack.is_empty s1 then ()
      else begin
        let j = Stack.pop s1 in
        let x = ref (i - j) in
        sum := !sum + !x;
        while not (Stack.is_empty s2) && (fst (Stack.top s2) > j) do
          x := !x + snd (Stack.pop s2);
        done;
        Stack.push (j, !x) s2;
      end
    | _ -> assert false);
  Printf.printf "%d\n" !sum;
  print_int (Stack.length s2);
  Stack.fold (fun acc x -> x :: acc) [] s2
  |> List.iter (fun (_, n) -> Printf.printf " %d" n);
  print_newline ()
