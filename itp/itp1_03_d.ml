Scanf.scanf"%d %d %d"(fun a b c->let x=ref 0in for i=a to b do if c mod i=0then x:=!x+1done;Printf.printf"%d
"!x)
