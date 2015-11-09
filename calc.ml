open Ast

let rec eval = function 
	Lit(x) -> "" ^ x
|	FunCall(x, y) -> ("" ^ x ^ "(" ^ eval y ^ ")")
|	While(x, y) -> "while ("^ eval x ^ ") {\n" ^ eval y ^ ";\n}"
|	Seq(v1, v2) -> "" ^ eval v1 ^ ";\n" ^ eval v2 ^ ";\n"
|	IntVarDec(v2, v3) -> "int " ^ v2 ^ " = " ^ eval v3 ^ ";\n"
|	Eq(v1, v2) -> "" ^ v1 ^ " == " ^ v2
|	Assn(v1, v2) -> "" ^ v1 ^ " = " ^ eval v2 ^ ";\n"

let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = eval expr in
	print_endline "#include <stdio.h>\n";
	print_endline "void print(char *x){printf(\"%s\",x);}\n";
	print_endline "int main(int argc, char **argv) {\n";
	print_endline result;
	print_endline "return 0;\n}"
