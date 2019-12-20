let g=print_newline;;let rec f()=Scanf.scanf"%d %d
"(fun h w->if h>0&&w>0then(for i=1to h do for j=1to w do print_char(if i>1&&i<h&&j<w&&j>1then '.'else '#')done;g()done;f(g())));;f()
