type vtype =
	Int | Char | Double | Bool | String | Any

type expr =
	Tree of expr * expr list
|	IntLit of string
|	ChrLit of string
|	FltLit of string
|	StrLit of string
|	GetBranch of expr * expr
|	GetWidth of expr
|	Void
|	FunCall of string * expr
|	Eq of expr * expr
|	Neq of expr * expr
|	Lt of expr * expr
|	Leq of expr * expr
|	Gt of expr * expr
|	Geq of expr * expr
|	Add of expr * expr
|	Minus of expr * expr
|	Mul of expr * expr
|	Div of expr * expr
|	Mod of expr * expr
|	Cast of vtype * expr
|	Id of string

type stmt =
	While of expr * stmt * stmt list
|	If of expr * stmt * stmt list
|	IfElse of expr * stmt * stmt * stmt list
|	FuncDec of string * vtype * string * stmt * stmt list
|	VarDec of vtype * string * expr
|	Assn of string * expr
|	Expr of expr
|	Return of expr
|	Seq of stmt list

type program = stmt list

let string_of_vtype = function
	Int -> "int"
| Char -> "char"
| Double -> "double"
| Bool -> "bool"
| String -> "string"
| Any -> "any"

let rec string_tab n v = if n == 0 then v else string_tab (n-1) ("\t" ^ v)

let rec tree_list_from_string =
	function s ->
		let len = String.length s in
		if (len > 0) then
			Tree(ChrLit("'" ^ (Char.escaped (String.get s 0)) ^ "'") , [])::
			tree_list_from_string (String.sub s 1 (len - 1))
		else
			[]

let rec gen_c_expr =
	let rec gen_c_tree_list = function n -> function
		hd::tl -> "" ^ (gen_c_expr n hd) ^ ", \n" ^ string_tab n (gen_c_tree_list n tl)
	|	[] -> ""
	in
	function n ->
	function
	FunCall(x, y) -> ("" ^ x ^ "(\n" ^ string_tab (n + 1) (gen_c_expr (n+1) y) ^ ")" )
|	Tree(IntLit(x), children) ->
		"int_treemake(" ^ x ^ ", " ^
			if List.length children == 0 then
				"NULL)"
			else
				"\n" ^ string_tab (n+1) (gen_c_tree_list (n+1) children ^ "NULL)")
|	Tree(ChrLit(x), children) ->
		"char_treemake(" ^ x ^ ", " ^
			if List.length children == 0 then
				"NULL)"
			else
				"\n" ^ string_tab (n+1) (gen_c_tree_list (n+1) children ^ "NULL)")
|	Tree(FltLit(x), children) ->
		"double_treemake(" ^ x ^ ", " ^
			if List.length children == 0 then
				"NULL)"
			else
				"\n" ^ string_tab (n+1) (gen_c_tree_list (n+1) children ^ "NULL)")
|	Tree(Void, children) ->
		"void_treemake(" ^
			if List.length children == 0 then
				"NULL)"
			else
				"\n" ^ string_tab (n+1) (gen_c_tree_list (n+1) children ^ "NULL)")
|	Tree(StrLit(x), []) -> "void_treemake(\n" ^
			string_tab (n+1) (gen_c_tree_list (n+1) (tree_list_from_string x) ^ "NULL)")
|	Tree(expr, children) ->
		"tree_treemake(" ^ gen_c_expr n expr ^ ", " ^
			if List.length children == 0 then
				"NULL)"
			else
				"\n" ^ string_tab (n+1) (gen_c_tree_list (n+1) children ^ "NULL)")
| GetBranch(tree, expr) -> "get_branch_t(" ^ gen_c_expr n tree ^ ", " ^ gen_c_expr n expr ^ ")"
|	GetWidth(tree) -> "get_width_t(" ^ gen_c_expr n tree ^ ")"
|	Eq(v1, v2) -> ("equal(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Neq(v1, v2) -> ("nequal(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Lt(v1, v2) -> ("lt(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Leq(v1, v2) -> ("lte(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Gt(v1, v2) -> ("gt(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Geq(v1, v2) -> ("gte(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Add(v1, v2) -> ("add(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Minus(v1, v2) -> ("sub(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Mul(v1, v2) -> ("mult(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Div(v1, v2) -> ("divd(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Mod(v1, v2) -> ("mod(" ^ gen_c_expr n v1 ^ ", " ^ gen_c_expr n v2 ^ ")")
|	Cast(vt,e) -> ("cast(" ^ string_of_vtype vt ^ ", " ^ gen_c_expr n e ^ ")")
|	Id(v1) -> "" ^ v1
|	IntLit(x) -> x
|	ChrLit(x) -> x
|	FltLit(x) -> x
|	StrLit(x) -> x
|	Void -> ""

let rec gen_c = function n -> function
	While(x, y, l) -> string_tab n ("while ("^(gen_c_expr n x) ^ ") { \n" ^ (gen_c (n+1) y) ^ "\n" ^  string_tab n "}")
|	If(x, y, l) -> string_tab n ("if ("^(gen_c_expr n x) ^ ") { \n" ^ (gen_c (n+1) y) ^ "\n" ^  string_tab n "}")
|	IfElse(x, s1,s2, l) -> 
		string_tab n ("if ("^(gen_c_expr n x) ^ ") { \n" ^ (gen_c (n+1) s1) ^ "\n" ^  string_tab n "} else {\n"
		^ (gen_c (n+1) s2) ^ "\n" ^ string_tab n "}")
|	VarDec(v1, v2, v3) -> string_tab n ("struct tree * " ^ v2 ^ " = " ^ gen_c_expr n v3 ^ "; inc_refcount(" ^ v2 ^ ");")
|	FuncDec(str, vt, vn, stmt, l) -> "" (*str ^ "(){\n" ^ gen_c (n + 1) stmt ^ "} " *)
|	Assn(v1, v2) -> string_tab n ("" ^ v1 ^ " = " ^ gen_c_expr n v2 ^ "; inc_refcount(" ^ v1 ^ ");")
|	Expr(v1) -> string_tab n (gen_c_expr n v1)
|	Return(v1) -> string_tab n ("return " ^ gen_c_expr n v1 ^ ";")
|	Seq(v1) -> let rec gen_c_seq = function
				hd::tl -> gen_c n hd ^ ";\n" ^ gen_c_seq tl
			|	[] -> "" in
		gen_c_seq v1

let rec eval_expr = function n ->
	let rec eval_tree_list = function n -> function
		hd::tl -> eval_expr n hd ^ "::\n" ^ string_tab (n) (eval_tree_list n tl)
	|	[] -> "[]"
	in
	function
	FunCall(x, y) -> ("FunCall(" ^ x ^ ", \n" ^ string_tab (n+1) ((eval_expr (n+1) y) ^ ")"))
|	Tree(StrLit(x), []) -> "Tree(Void, \n" ^ string_tab (n+1) (eval_tree_list (n+1) (tree_list_from_string x) ^ ")")
|	Tree(expr, children) ->
		"Tree(" ^ eval_expr n expr ^
			if List.length children == 0 then
				", [])"
			else
				", \n" ^ string_tab (n+1) (eval_tree_list (n+1) children ^ ")")
|	GetBranch(tree, expr) -> "" (* TODO *)
|	GetWidth(tree) -> ""
|	Eq(v1, v2) -> ("Eq(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Neq(v1, v2) -> ("Neq(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Lt(v1, v2) -> ("Lt(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Leq(v1, v2) -> ("Leq(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Gt(v1, v2) -> ("Gt(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Geq(v1, v2) -> ("Geq(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Add(v1, v2) -> ("Add(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Minus(v1, v2) -> ("Minus(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Mul(v1, v2) -> ("Mul(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Div(v1, v2) -> ("Div(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Mod(v1, v2) -> ("Mod(" ^ eval_expr n v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Cast(vt,e) -> ("Cast(" ^ string_of_vtype vt ^ ", " ^ eval_expr n e ^ ")")
|	Id(v1) -> "Id(" ^ v1 ^ ")"
|	IntLit(x) -> x
|	ChrLit(x) -> x
|	FltLit(x) -> x
|	StrLit(x) -> x
|	Void -> "Void"

let rec eval = function n -> function
	While(x, y, l) -> string_tab n ("While("^(eval_expr n x) ^ ",\n" ^ (eval (n+1) y) ^ "\n" ^  string_tab n ")")
|	If(x, y, l) -> string_tab n ("If("^(eval_expr n x) ^ ",\n" ^ (eval (n+1) y) ^ "\n" ^  string_tab n ")")
|	IfElse(x, s1, s2, l) -> string_tab n ("IfElse("^(eval_expr n x) ^ ",\n" ^ 
			(eval (n+1) s1) ^ "\n" ^  string_tab n ")\n" ^
			(eval (n+1) s2) ^ "\n" ^  string_tab n ")")
|	VarDec(v1, v2, v3) -> string_tab n ("VarDec(" ^ v2 ^ ", " ^ eval_expr n v3 ^ ")")
|	FuncDec(str, vt, vn, stmt, l) -> "" (* TODO *)
|	Assn(v1, v2) -> string_tab n ("Assn(" ^ v1 ^ ", " ^ eval_expr n v2 ^ ")")
|	Expr(v1) -> string_tab n (eval_expr n v1)
|	Return(v1) -> string_tab n ("Return(" ^ eval_expr n v1 ^ ")")
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


let rec gen_c_funcs = function
	FuncDec(str, vt, vn, stmt, l)::tl -> "struct tree *" ^ str ^ "(struct tree *" ^ vn ^ "){" ^
	"inc_refcount(" ^ vn ^ ");\n" ^ gen_c 1 stmt ^ "return NULL;}\n" ^ gen_c_funcs tl
|	_::tl -> gen_c_funcs tl
|	[] -> ""

let headers =
	"#include <stdio.h>\n#include <stdlib.h>\n" ^
	"#include <stdio.h>\n" ^
	"#include <stdlib.h>\n" ^
	"#include <string.h>\n" ^
	"#include <stdarg.h>\n"

let string_of_program stmts =
	"int main(int argc, char **argv) {\n" ^
	gen_c_prog stmts ^
	"\treturn 0;\n}"
