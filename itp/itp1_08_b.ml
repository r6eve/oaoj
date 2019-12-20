let rec f()=let s=read_line()in if s<>"0"then let x=ref 0in Bytes.iter(fun c->x:=!x+Char.code c-48)s;f(Printf.printf"%d
"!x);;f()
