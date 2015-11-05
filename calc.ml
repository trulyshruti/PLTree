open Ast

let strip_both_chars str =
  match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)

let rec eval = function 
	Lit(x) -> print_endline x
|	Int(x) -> print_endline (string_of_int (x/2))
|	Flt(x) -> print_endline (string_of_float (sqrt x))
|	Str(x) -> print_endline x
|	FunCall(x, y) -> print_endline (strip_both_chars y)
|	Seq(v1, v2) -> eval v1; eval v2


let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = eval expr in
	result
