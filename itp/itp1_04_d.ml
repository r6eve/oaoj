open List let _=read_int()and l=map int_of_string(Str.split(Str.regexp" ")(read_line()));;Printf.printf"%d %d %d
"(fold_left min max_int l)(fold_left max min_int l)(fold_left(+)0 l)
