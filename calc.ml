open Ast
open Printf

let rec string_tab n v = if n == 0 then v else string_tab (n-1) ("\t" ^ v)





let rec gen_c_expr = 
	let rec gen_c_tree_list = function
		hd::tl -> gen_c_expr hd ^ ", \n" ^ gen_c_tree_list tl
	|	[] -> ""
	in
	function 
	FunCall(x, y) -> ("" ^ x ^ "(" ^  (gen_c_expr y) ^ ")")
|	Tree(IntLit(x), children) -> 
		"int_treemake(" ^ x ^ ", " ^ gen_c_tree_list children ^ "NULL)"
|	Tree(ChrLit(x), children) -> 
		"char_treemake(" ^ x ^ ", " ^ gen_c_tree_list children ^ "NULL)"
|	Tree(FltLit(x), children) -> 
		"double_treemake(" ^ x ^ ", " ^ gen_c_tree_list children ^ "NULL)"
|	Tree(StrLit(x), children) -> "tree_of_string(" ^ x ^ ")"
|	Tree(expr, children) -> 
		"tree_treemake(" ^ gen_c_expr expr ^ ", " ^ gen_c_tree_list children ^ "NULL)"
|	Eq(v1, v2) -> ("equal(" ^ gen_c_expr v1 ^ ", " ^ gen_c_expr v2 ^ ")")
|	Lt(v1, v2) -> ("lt(" ^ gen_c_expr v1 ^ ", " ^ gen_c_expr v2 ^ ")")
|	Add(v1, v2) -> ("add(" ^ gen_c_expr v1 ^ ", " ^ gen_c_expr v2 ^ ")")
|	Id(v1) -> "" ^ v1
|	IntLit(x) -> x
|	ChrLit(x) -> x
|	FltLit(x) -> x
|	StrLit(x) -> x

let rec gen_c = function n -> function
	While(x, y) -> string_tab n ("while ("^(gen_c_expr x) ^ ") { \n" ^ (gen_c (n+1) y) ^ "\n" ^  string_tab n "}")
|	VarDec(v2, v3) -> string_tab n ("struct tree * " ^ v2 ^ " = " ^ gen_c_expr v3 ^ ";")
|	Assn(v1, v2) -> string_tab n ("" ^ v1 ^ " = " ^ gen_c_expr v2 ^ ";")
|	Expr(v1) -> string_tab n (gen_c_expr v1)
|	Seq(v1) -> let rec gen_c_seq = function
				hd::tl -> gen_c n hd ^ ";\n" ^ gen_c_seq tl
			|	[] -> "" in
		gen_c_seq v1




let rec eval_expr =
	let rec eval_tree_list = function
		hd::tl -> eval_expr hd ^ "::" ^ eval_tree_list tl
	|	[] -> "[]"
	in
	function 
	FunCall(x, y) -> ("" ^ x ^ "(" ^  (eval_expr y) ^ ")")
|	Tree(expr, children) -> 
		"Tree(" ^ eval_expr expr ^ ", " ^ eval_tree_list children ^ ")"
|	Eq(v1, v2) -> ("Eq(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Lt(v1, v2) -> ("Lt(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Add(v1, v2) -> ("Add(" ^ eval_expr v1 ^ ", " ^ eval_expr v2 ^ ")")
|	Id(v1) -> "Id(" ^ v1 ^ ")"
|	IntLit(x) -> x
|	ChrLit(x) -> x
|	FltLit(x) -> x
|	StrLit(x) -> x

let rec eval = function n -> function
	While(x, y) -> string_tab n ("While("^(eval_expr x) ^ ",\n" ^ (eval (n+1) y) ^ "\n" ^  string_tab n ")")
|	VarDec(v2, v3) -> string_tab n ("VarDec(" ^ v2 ^ ", " ^ eval_expr v3 ^ ")")
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
	hd::tl -> "" ^ gen_c 1 hd ^ ";\n" ^ gen_c_prog tl
|	[] -> ""

let print_func = "void print(struct tree *str) {\n" ^ 
		 "	int width = str->width;\n" ^ 
		 "	int i = 0;\n" ^
		 "	while (i < width) {\n " ^
		 "		putchar(*(char *)(get_branch(str, i)->data)); \n" ^
		 "		i++; \n" ^ 
		 "	}\n" ^
		 "	putchar('\\n');\n " ^
		 "}\n"

let get_c = function
	prog -> "#include <stdio.h>\n#include <stdlib.h>\n#include \"tree.h\"\n" ^
		"int main(int argc, char **argv) {\n" ^
		gen_c_prog prog ^
		"\treturn 0;\n}"

let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	let result = if (Array.length Sys.argv > 1 && Sys.argv.(1) = "-c") then get_c program else eval_prog program in
	print_endline result;
