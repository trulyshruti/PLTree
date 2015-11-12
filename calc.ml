open Ast

let rec eval_expr = function 
	Lit(x) -> "Lit(" ^ x ^ ")"
|	FunCall(x, y) -> "FunCall(" ^ x ^ ", " ^  eval_expr y ^ ")"
|	Eq(v1, v2) -> "Eq(" ^ v1 ^ ", " ^ v2 ^ ")"
|	Lt(v1, v2) -> "Lt(" ^ v1 ^ ", " ^ v2 ^ ")"
|	Add(v1, v2) -> "Add(" ^ v1 ^ ", " ^ v2 ^ ")"

let rec eval = function
	While(x, y) -> "While("^ eval_expr x ^ ", " ^ eval y ^ ")"
|	VarDec(v1, v2, v3) -> (match v1 with
				Int -> "VarDec(Int, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")"
			|	Char -> "VarDec(Char, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")"
			|	Double -> "VarDec(Double, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")"
			|	Bool -> "VarDec(Bool, " ^ v2 ^ ", " ^ eval_expr v3 ^ ")")
|	Assn(v1, v2) -> "Assn(" ^ v1 ^ ", " ^ eval_expr v2 ^ ")"
|	Expr(v1) -> eval_expr v1
|	Seq(v1) -> let rec eval_seq = function
				hd::tl -> "\n\t" ^ eval hd ^ eval_seq tl
			|	[] -> "" in
		eval_seq v1

let rec eval_prog = function
	hd::tl -> "" ^ eval hd ^ "\n" ^ eval_prog tl
|	[] -> ""

let _ = 
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	let result = eval_prog program in
	print_endline result;
