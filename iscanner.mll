{ open Iparser }

rule token = parse
 (*[' '] { token lexbuf }*)
| "/*"     { comment lexbuf }
| [^'$''"'] as lxm { CHAR_LITERAL(lxm) }
| '$' { let buffer = Buffer.create 1 in OPEN (imp buffer lexbuf) }
| '"' { let buffer = Buffer.create 1 in STRING_LITERAL (stringl buffer lexbuf) }
| eof { EOF }
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
and imp buffer = parse
 | '$' { Buffer.contents buffer }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; imp buffer lexbuf }
