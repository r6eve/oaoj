let f=Scanf.scanf open List;;let s(a,b)=(b,a);;let($)d c=match d with|[x;y;z]->(match c with|'N'->[y;s x;z]|'S'->[s y;x;z]|'E'->[s z;y;x]|'W'->[z;y;s x]|'Z'->[x;z;s y]|_->[])|_->[]and z=f"%d %d %d %d %d %d\n"(fun a b c d e f->[(a,f);(b,e);(c,d)]);;for _=1to f"%d
"(fun i->i)do f"%d %d
"(fun x y->Printf.printf"%d
"(fst(nth(hd(filter(fun d->x=(fst(hd d))&&y=(fst(nth d 1)))(concat(map(fun d->[d;d$'Z';d$'Z'$'Z';d$'Z'$'Z'$'Z'])[z;z$'N';z$'W';z$'E';z$'S';z$'S'$'S']))))2)))done
