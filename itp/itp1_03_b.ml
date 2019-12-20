let rec f?(n=read_int())i=if n>0then f(Printf.printf"Case %d: %d
"i n;i+1);;f 1
(*
let rec f?(n=read_int())i=n>0&f(Printf.printf"Case %d: %d
"i n;i+1);;f 1
*)
