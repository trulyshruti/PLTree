{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ['a'-'z']+ as lxm { LITERAL(lxm) }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| eof {EOF}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
