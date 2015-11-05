type literal =
	Flt of float
|	Str of string
|	Int of int

type expr =
	Lit of string
|	Int of int
|	Str of string
|	Flt of float
|	FunCall of string * string
|	Seq of expr * expr
