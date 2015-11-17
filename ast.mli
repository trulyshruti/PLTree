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
|	Lt of expr * expr
|	Add of expr * expr
|	Id of string

type stmt = 
	While of expr * stmt
|	VarDec of string * expr
|	Assn of string * expr
|	Expr of expr
|	Seq of stmt list




type program = stmt list
