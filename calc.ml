open Ast
open Printf

let rec string_tab n v = if n == 0 then v else string_tab (n-1) ("\t" ^ v)





let rec gen_c_expr = function 
	Lit(x) -> x
|	FunCall(x, y) -> ("" ^ x ^ "(" ^  (gen_c_expr y) ^ ")")
|	Eq(v1, v2) -> ("(" ^ gen_c_expr v1 ^ ") == (" ^ gen_c_expr v2 ^ ")")
|	Lt(v1, v2) -> ("(" ^ gen_c_expr v1 ^ ") < (" ^ gen_c_expr v2 ^ ")")
|	Add(v1, v2) -> ("(" ^ gen_c_expr v1 ^ ") + (" ^ gen_c_expr v2 ^ ")")
|	Id(v1) -> v1

let rec gen_c = function n -> function
	While(x, y) -> string_tab n ("while ("^(gen_c_expr x) ^ ") { \n" ^ (gen_c (n+1) y) ^ "\n" ^  string_tab n "}")
|	VarDec(v1, v2, v3) -> (match v1 with
				Int -> string_tab n ("int " ^ v2 ^ " = " ^ gen_c_expr v3 ^ ";")
			|	Char -> string_tab n ("char " ^ v2 ^ " = " ^ gen_c_expr v3 ^ ";")
			|	Double -> string_tab n ("double " ^ v2 ^ ", " ^ gen_c_expr v3 ^ ";")
			|	Bool -> string_tab n ("bool " ^ v2 ^ " = " ^ gen_c_expr v3 ^ ";"))
|	Assn(v1, v2) -> string_tab n ("" ^ v1 ^ " = " ^ gen_c_expr v2 ^ ";")
|	Expr(v1) -> string_tab n (gen_c_expr v1)
|	Seq(v1) -> let rec gen_c_seq = function
				hd::tl -> gen_c n hd ^ ";\n" ^ gen_c_seq tl
			|	[] -> "" in
		gen_c_seq v1

let rec gen_c_prog = function
	hd::tl -> "" ^ gen_c 0 hd ^ "\n" ^ gen_c_prog tl
|	[] -> ""





let rec eval_expr = function 
	Lit(x) -> "Lit(" ^ x ^ ")"
|	FunCall(x, y) -> ("FunCall(" ^ x ^ ", " ^  (eval_expr y) ^ ")")
|	Eq(v1, v2) -> ("Eq(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Lt(v1, v2) -> ("Lt(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Add(v1, v2) -> ("Add(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Id(v1) -> v1

let rec eval = function n -> function
	While(x, y) -> string_tab n ("While("^(eval_expr x) ^ ", \n" ^ (eval (n+1) y) ^ "\n" ^  string_tab n ")")
|	VarDec(v1, v2, v3) -> (match v1 with
				Int -> string_tab n ("VarDec(Int, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")")
			|	Char -> string_tab n ("VarDec(Char, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")")
			|	Double -> string_tab n ("VarDec(Double, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")")
			|	Bool -> string_tab n ("VarDec(Bool, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")"))
|	Assn(v1, v2) -> string_tab n ("Assn(" ^ v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Expr(v1) -> string_tab n (eval_expr v1)
|	Seq(v1) -> let rec eval_seq = function
				hd::tl -> eval n hd ^ "\n" ^ eval_seq tl
			|	[] -> "" in
		eval_seq v1

let rec eval_prog = function
	hd::tl -> "" ^ eval 0 hd ^ "\n" ^ eval_prog tl
|	[] -> ""


let rec gen_c_prog = function
	hd::tl -> "" ^ gen_c 1 hd ^ "\n" ^ gen_c_prog tl
|	[] -> ""

let get_c = function
	prog -> "#include <stdio.h>\n#include <stdlib.h>\n#include \"tree.h\"\n" ^
		"void print(char *str){printf(\"%s\", str);}\n" ^
		"int main(int argc, char **argv) {\n" ^
		gen_c_prog prog ^
		"}"

let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	let result = if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-c") then get_c program else eval_prog program in
	print_endline result;
