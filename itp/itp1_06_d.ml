let f()=List.map int_of_string(Str.split(Str.regexp" ")(read_line()))open Array;;match f()with[n;m]->let a,b=make(n*m)0,make m 0in for i=0to n-1do List.iteri(fun j e->a.(m*i+j)<-e)(f())done;for i=0to m-1do b.(i)<-read_int()done;for i=0to n-1do let c=ref 0in for j=0to m-1do c:=!c+a.(m*i+j)*b.(j)done;Printf.printf"%d
"!c done|_->()
