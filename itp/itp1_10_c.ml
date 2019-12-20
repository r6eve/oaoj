let g=List.fold_left(fun a s->a+.s)0.;;let rec f()=let n=read_float()in if n>0.then let l=List.map float_of_string(Str.split(Str.regexp" ")(read_line()))in f(Printf.printf"%f
"(sqrt(g(List.map(fun s->(s-.(g l/.n))**2.)l)/.n)));;f()
