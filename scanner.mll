{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('     		{ LPAREN }
| ')'     		{ RPAREN }
| '{'     		{ LBRACE }
| '}'     		{ RBRACE }
| ','      		{ COMMA }
| '+'      		{ PLUS }
| '-'      		{ MINUS }
| '*'      		{ TIMES }
| '/'      		{ DIVIDE }
| '='      		{ ASSIGN }
| "=="     		{ EQ }
| "!="     		{ NEQ }
| '<'     		{ LT }
| "<="    		{ LEQ }
| ">"     		{ GT }
| ">="    		{ GEQ }
| "if"     		{ IF }
| "ifelse" 		{ IFELSE }
| "while"  		{ WHILE }
| "return" 		{ RETURN }
| "int" 	   	{ INT }
| "char" 	  	{ CHAR }
| "bool"  	 	{ BOOL }
| "void"   		{ VOID }
| "double" 		{ DOUBLE }
| ['0'-'9']+ as lxm 	{ LITERAL(lxm) }
| ['0'-'9']+'.'['0'-'9']+ as lxm { LITERAL(lxm) }
| '"'([^'\n''"']|"\\\"")*'"' as lxm { LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
