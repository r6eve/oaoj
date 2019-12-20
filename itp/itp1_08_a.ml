open Char;;Bytes.iter(fun c->print_char(if c>'@'&&c<'['then chr(code c+32)else if c>'`'&&c<'{'then chr(code c-32)else c))(read_line());print_newline()
