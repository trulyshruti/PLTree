{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
