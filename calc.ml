open Ast

let strip_both_chars str =
  match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)

let rec eval = function 
	Lit(x) -> x
|	Int(x) -> (string_of_int (x/2))
|	Flt(x) -> (string_of_float (sqrt x))
|	Str(x) -> x
|	FunCall("print", y) -> ("printf(" ^ y ^ ");\n")
|	FunCall(x, y) -> ("" ^ x ^ "(" ^ y ^ ");\n")
|	Seq(v1, v2) -> (eval v1) ^ (eval v2)


let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = eval expr in
	print_endline "#include <stdio.h>\n";
	print_endline "int main(int argc, char **argv) {\n";
	print_endline result;
	print_endline "}"
