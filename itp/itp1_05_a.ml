let rec f()=Scanf.scanf"%d %d
"(fun h w->if h>0&&w>0then(for _=1to h do print_endline(Bytes.make w '#')done;f(print_newline())));;f()
