let s(a,b)=(b,a)open List;;let($)d c=match d with|[x;y;z]->(match c with|'N'->[y;s x;z]|'S'->[s y;x;z]|'E'->[s z;y;x]|'W'->[z;y;s x]|'Z'->[x;z;s y]|_->[])|_->[]and f()=Scanf.scanf"%d %d %d %d %d %d
"(fun a b c d e f->[(a,f);(b,e);(c,d)]);;let z=f();;print_endline(if mem(f())(concat(map(fun d->[d;d$'Z';d$'Z'$'Z';d$'Z'$'Z'$'Z'])[z;z$'N';z$'W';z$'E';z$'S';z$'S'$'S']))then"Yes"else"No")
