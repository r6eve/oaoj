let rec f()=Scanf.scanf"%d %c %d
"(fun a o b->if o<'0'then f(Printf.printf"%d
"(match o with|'+'->a+b|'-'->a-b|'*'->a*b|'/'->a/b|_->0)));;f()
