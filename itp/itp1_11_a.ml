let f(a,b)=(b,a)and s=ref(Scanf.scanf"%d %d %d %d %d %d
"(fun a b c d e f->[(a,f);(b,e);(c,d)]));;Bytes.iter(fun c->s:=match!s with[x;y;z]->(match c with|'N'->[y;f x;z]|'S'->[f y;x;z]|'E'->[f z;y;x]|'W'->[z;y;f x]|_->[])|_->[])(Scanf.scanf"%s"(fun l->l));Printf.printf"%d
"(fst(List.hd!s))
