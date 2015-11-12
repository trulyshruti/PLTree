
type vtype =
	Int | Char | Double | Bool

type expr =
	Lit of string
|	FunCall of string * expr
|	Eq of string * string
|	Lt of string * string
|	Add of string * string

type stmt = 
	While of expr * stmt
|	VarDec of vtype * string * expr
|	Assn of string * expr
|	Expr of expr
|	Seq of stmt list


type program = stmt list
