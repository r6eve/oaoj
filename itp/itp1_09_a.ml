let g,w=Bytes.map(fun c->if c>'@'&&c<'['then Char.chr(Char.code c+32)else c),read_line()open List;;let rec f n=match read_line()with"END_OF_TEXT"->Printf.printf"%d
"n|t->f(n+length(filter(fun s->s=(g w))(Str.split(Str.regexp" ")(g t))));;f 0
