let n=read_int()open List;;let l()=map float_of_string(Str.split(Str.regexp" ")(read_line()));;let x,y=l(),l();;let m p=exp(log(fold_left2(fun a b c->a+.abs_float(b-.c)**p)0. x y)/.p);;Printf.printf"%f
%f
%f
%f
"(m 1.)(m 2.)(m 3.)(fold_left2(fun a b c->max a(abs_float(b-.c)))0. x y)
