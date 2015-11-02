open Ast

let rec eval = function 
	Lit(x) -> x/2

let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = eval expr in
	print_endline (string_of_int result ^ " ")
