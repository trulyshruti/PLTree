type vtype =
	Int | Char | Double | Bool | String

type expr =
	Tree of expr * expr list
|	IntLit of string
|	ChrLit of string
|	FltLit of string
|	StrLit of string
|	GetBranch of expr * expr
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
|	Id of string

type stmt =
	While of expr * stmt * stmt list
|	If of expr * stmt * stmt list
|	FuncDec of string * stmt * stmt list
|	VarDec of string * expr
|	Assn of string * expr
|	Expr of expr
|	Return of expr
|	Seq of stmt list

type program = stmt list

let rec get_vars_list = function
	[] -> []
	| hd::tl -> match hd with VarDec(_,_) -> hd::get_vars_list tl
		| _ -> get_vars_list tl

let rec get_funcs_list = function
	[] -> []
	| hd::tl -> match hd with FuncDec(_,Seq(l),_) -> List.concat
		[ get_funcs_list l; hd::get_funcs_list tl ]
		| While(_,Seq(l),_) -> List.concat
		[get_funcs_list l; get_funcs_list tl ]
		| If(_,Seq(l),_) -> List.concat
		[get_funcs_list l; get_funcs_list tl ]
		|_ -> get_funcs_list tl

let string_of_vtype = function
	Int -> "int"
| Char -> "char"
| Double -> "double"
| Bool -> "bool"
| String -> "string"

let rec string_of_expr = function
	Tree(e,l) -> string_of_expr e ^ if List.length l > 0 then" {{ " ^
		String.concat ", " (List.map string_of_expr l) ^ "}}" else "{{}}"
| IntLit(s) -> s
| ChrLit(s) -> s
| FltLit(s) -> s
| StrLit(s) -> s
| GetBranch(e1,e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
| Void -> "void"
| FunCall(s,e) -> s ^ "(" ^ string_of_expr e ^ ")"
| Eq(e1, e2) -> string_of_expr e1 ^ " == " ^ string_of_expr e2
| Neq(e1, e2) -> string_of_expr e1 ^ " != " ^ string_of_expr e2
| Lt(e1, e2) -> string_of_expr e1 ^ " < " ^ string_of_expr e2
| Leq(e1, e2) -> string_of_expr e1 ^ " <= " ^ string_of_expr e2
| Gt(e1, e2) -> string_of_expr e1 ^ " > " ^ string_of_expr e2
| Geq(e1, e2) -> string_of_expr e1 ^ " >= " ^ string_of_expr e2
| Add(e1, e2) -> string_of_expr e1 ^ " + " ^ string_of_expr e2
| Minus(e1, e2) -> string_of_expr e1 ^ " - " ^ string_of_expr e2
| Mul(e1, e2) -> string_of_expr e1 ^ " * " ^ string_of_expr e2
| Div(e1, e2) -> string_of_expr e1 ^ " / " ^ string_of_expr e2
| Mod(e1, e2) -> string_of_expr e1 ^ " % " ^ string_of_expr e2
| Id(s) -> s

let rec string_of_stmt = function
	While(e,s,l) -> string_of_expr e ^ " " ^ string_of_stmt s
| If(e,s,l) -> string_of_expr e ^ " " ^ string_of_stmt s
| FuncDec(str,stmt,l) -> str ^ "[ " ^ string_of_stmt stmt ^ "] "
| VarDec(s,e) -> s ^ " " ^ string_of_expr e
| Assn(s,e) -> s ^ " = " ^ string_of_expr e
| Expr(e) -> string_of_expr e
| Return(e) -> "return:(" ^ string_of_expr e ^ ")";
| Seq(l) ->  String.concat ", " (List.map string_of_stmt l)

let string_of_program stmts =
	String.concat " " (List.map string_of_stmt stmts)
