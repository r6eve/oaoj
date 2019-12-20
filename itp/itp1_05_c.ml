let g=print_newline;;let rec f()=Scanf.scanf"%d %d
"(fun h w->if h>0&&w>0then(for i=1to h do let b=ref(i mod 2=0)in for j=1to w do print_char(if!b then '.'else '#');b:=not!b;done;g()done;f(g())));;f()
