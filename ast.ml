type vtype =
	Int | Char | Double | Bool | String

type expr =
	Tree of expr * expr list
|	IntLit of string
|	ChrLit of string
|	FltLit of string
|	StrLit of string
|	Void
|	FunCall of string * expr
|	Eq of expr * expr
| Neq of expr * expr
|	Lt of expr * expr
| Leq of expr * expr
| Gt of expr * expr
| Geq of expr * expr
|	Add of expr * expr
| Minus of expr * expr
| Mul of expr * expr
| Div of expr * expr
|	Id of string

type stmt =
	While of expr * stmt
|	VarDec of string * expr
|	Assn of string * expr
|	Expr of expr
|	Seq of stmt list

type program = stmt list

let string_of_vtype = function
	Int -> "int"
| Char -> "char"
| Double -> "double"
| Bool -> "bool"
| String -> "string"

let rec string_of_expr = function
	Tree(e,l) -> string_of_expr e ^ " {{ " ^
		String.concat ", " (List.map string_of_expr l) ^ "}}"
| IntLit(s) -> s
| ChrLit(s) -> s
| FltLit(s) -> s
| StrLit(s) -> s
| Void -> "void"
| FunCall(s,e) -> s ^ " " ^ string_of_expr e
| Eq(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Neq(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Lt(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Leq(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Gt(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Geq(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Add(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Minus(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Mul(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Div(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Id(s) -> s

let rec string_of_stmt = function
	While(e,s) -> string_of_expr e ^ " " ^ string_of_stmt s
| VarDec(s,e) -> s ^ " " ^ string_of_expr e
| Assn(s,e) -> s ^ " " ^ string_of_expr e
| Expr(e) -> string_of_expr e
| Seq(l) ->  String.concat ", " (List.map string_of_stmt l)

let string_of_program stmts =
  String.concat " " (List.map string_of_stmt stmts)
