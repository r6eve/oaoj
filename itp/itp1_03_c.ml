let rec f()=Scanf.scanf"%d %d
"(fun x y->if x>0||y>0then f(Printf.printf"%d %d
"(min x y)(max x y)));;f()
