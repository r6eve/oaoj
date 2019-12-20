open Hashtbl;;let t,b=create 26,Bytes.iter;;let rec f()=let l=read_line()in b(fun c->if c>'@'&&c<'['||c>'`'&&c<'{'then let x=(if c>'@'&&c<'['then Char.chr(Char.code c+32)else c)in if mem t x then add t x(find t x+1)else add t x 1)l;f()in try f()with _->b(fun c->Printf.printf"%c : %d
"c(if mem t c then find t c else 0))"abcdefghijklmnopqrstuvwxyz"
