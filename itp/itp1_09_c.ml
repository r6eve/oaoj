let n,x,y=read_int(),ref 0,ref 0;;for _=1to n do Scanf.scanf"%s %s
"(fun a b->if a=b then(incr x;incr y)else if a<b then y:=!y+3else x:=!x+3)
done;Printf.printf"%d %d
"!x !y
