type vtype =
	Int | Char | Double | Bool

type expr =
	Lit of string
|	FunCall of string * expr
|	Eq of expr * expr
|	Lt of expr * expr
|	Add of expr * expr
|	Id of string

type stmt =
	While of expr * stmt
|	VarDec of vtype * string * expr
|	Assn of string * expr
|	Expr of expr
|	Seq of stmt list

type program = stmt list

let string_of_vtype = function
	Int -> "int"
| Char -> "char"
| Double -> "double"
| Bool -> "bool"

let rec string_of_expr = function
	Lit(l) -> l
| FunCall(s,e) -> s ^ " " ^ string_of_expr e
| Eq(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Lt(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Add(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
| Id(s) -> s

let rec string_of_stmt = function
	While(e,s) -> string_of_expr e ^ " " ^ string_of_stmt s
| VarDec(t,s,e) -> string_of_vtype t ^ " " ^ s ^ " " ^ string_of_expr e
| Assn(s,e) -> s ^ " " ^ string_of_expr e
| Expr(e) -> string_of_expr e
| Seq(l) ->  String.concat ", " (List.map string_of_stmt l)

let string_of_program stmts =
  String.concat " " (List.map string_of_stmt stmts)
