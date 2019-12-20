let s=ref(read_line())open Bytes;;for _=1to read_int()do match Scanf.scanf"%s "(fun i->i)with"print"->Scanf.scanf"%d %d
"(fun a b->print_endline(sub!s a(b-a+1)))|"reverse"->Scanf.scanf"%d %d
"(fun a b->s:=mapi(fun i c->if i>=a&&i<=b then!s.[a+b-i]else c)!s)|_->Scanf.scanf"%d %d %s
"(fun a b p->s:=sub!s 0a^p^sub!s(b+1)(length!s-1-b))done
