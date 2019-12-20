open Bytes;;let rec f()=let s=ref(read_line())in if!s<>"-"then(for _=1to read_int() do let h=read_int()in s:=sub!s h(length!s-h)^sub!s 0h;done;f(print_endline!s));;f()
