let z=(-1);;let rec g()=Scanf.scanf"%d %d %d
"(fun m f r->if m>z||f>z||r>z then g(print_endline(if m=z||f=z then"F"else let x=m+f in if x>79then"A"else if x>64then"B"else if x>49||x>29&&r>49then"C"else if x>29then"D"else"F")));;g()
