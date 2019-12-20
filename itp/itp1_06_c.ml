let a,n,x=Array.make(4*3*10)0,read_int(),10;;for _=1to n do Scanf.scanf"%d %d %d %d
"(fun b f r v->let y=3*x*(b-1)+x*f-x+r-1 in a.(y)<-a.(y)+v)done;for b=1to 4do for f=1to 3do for r=1to 10do Printf.printf" %d"a.(3*x*(b-1)+x*f-x+r-1)done;print_newline()done;if b<4then print_endline(Bytes.make 20'#')done
