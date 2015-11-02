type expr =
	Lit of string
|	Int of int
|	Seq of expr * expr
