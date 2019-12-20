let rec f()=Scanf.scanf"%d %d
"(fun n x->if n>0||x>0then let z=ref 0in for a=1to n do for b=a+1to n do if b<x-a-b&&x-a-b<=n then z:=!z+1done done;f(Printf.printf"%d
"!z));;f()
