type expr =
	Lit of string
|	FunCall of string * expr
|	While of expr * expr
|	Seq of expr * expr
|	IntVarDec of string * expr
