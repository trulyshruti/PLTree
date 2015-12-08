{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('     		{ LPAREN }
| ')'     		{ RPAREN }
| '{'     		{ LBRACE }
| '}'     		{ RBRACE }
| '['			{ LBRACK }
| ']'			{ RBRACK }
| '.'			{ DOT }
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
| ":"			{ COL }
| ";"			{ SEMI }
| "if"     		{ IF }
| "ifelse" 		{ IFELSE }
| "while"  		{ WHILE }
| "return" 		{ RETURN }
| "int" 	   	{ INT }
| "char" 	  	{ CHAR }
| "bool"  	 	{ BOOL }
| "void"   		{ VOID }
| "double" 		{ DOUBLE }
| "string"		{ STRING }
| "'"[^'\n']"'" as lxm	{ CHAR_LITERAL(lxm) }
| "'\\n'" 	as lxm 	{ CHAR_LITERAL(lxm) }
| '-'?['0'-'9']+ as lxm 	{ INT_LITERAL(lxm) }
| '-'?['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_LITERAL(lxm) }
| '"' { let buffer = Buffer.create 1 in STRING_LITERAL (stringl buffer lexbuf) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
and  stringl buffer = parse
 | '"' { Buffer.contents buffer }
 | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
 | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
