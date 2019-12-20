let () =
  try
    while true do
      let x = ref 0 in
      let n = read_int () in
      for a = 0 to 9 do
        for b = 0 to 9 do
          for c = 0 to 9 do
            for d = 0 to 9 do
              if a + b + c + d = n then incr x;
            done
          done
        done
      done;
      Printf.printf "%d\n" !x;
    done
  with _ -> ()
