open Sast

type prog_els = { variables: Cast.stmt list;
	functions: Cast.stmt list;
	all_statements: Cast.stmt list; }

(* turns S-ast to C-ast*)
let transform prog =
	let rec expr = function
	Tree(e,l) -> let l = List.map (fun e -> expr e) l in Cast.Tree(expr e,l)
	| IntLit(s) -> Cast.IntLit(s)
	| ChrLit(s) -> Cast.ChrLit(s)
	| FltLit(s) -> Cast.FltLit(s)
	| StrLit(s) -> Cast.StrLit(s)
	| GetBranch(e1,e2) -> Cast.GetBranch(expr e1, expr e2)
	| Void -> Cast.Void
	| FunCall(s,e) -> Cast.FunCall(s, expr e)
	| Eq(e1,e2) -> Cast.Eq(expr e1,expr e2)
	| Neq(e1,e2) -> Cast.Neq(expr e1,expr e2)
	| Lt(e1,e2) -> Cast.Lt(expr e1,expr e2)
	| Leq(e1,e2) -> Cast.Leq(expr e1,expr e2)
	| Gt(e1,e2) -> Cast.Gt(expr e1,expr e2)
	| Geq(e1,e2) -> Cast.Geq(expr e1,expr e2)
	| Add(e1,e2) -> Cast.Add(expr e1,expr e2)
	| Minus(e1,e2) -> Cast.Minus(expr e1,expr e2)
	| Mul(e1,e2) -> Cast.Mul(expr e1,expr e2)
	| Div(e1,e2) -> Cast.Div(expr e1,expr e2)
	| Id(s) -> Cast.Id(s) in

	let rec stmt = function
	While(e,s,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.While(expr e, stmt s, l)
	| FuncDec(s,seq,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.FuncDec(s, stmt seq, l)
	| VarDec(s,e) -> Cast.VarDec(s,expr e)
	| Assn(s,e) -> Cast.Assn(s,expr e)
	| Expr(e) -> Cast.Expr(expr e)
	| Seq(l) -> let l = List.map (fun s -> stmt s) l in
	Cast.Seq(l) in

	(* Get all funcs and top-level vars *)
	let vars = Sast.get_vars_list prog in
	let funcs = Sast.get_funcs_list prog in {
	variables = List.map (fun v -> stmt v) vars;
	functions = List.map (fun f -> stmt f) funcs;
	all_statements = List.map	(fun s -> stmt s) prog; }

(* Use the string_of funcs in cast.ml to make C file *)
let execute_prog prog = let pe = transform prog in
	print_endline ("Vars: " ^ Cast.gen_c_prog pe.variables ^
	"\n\nFuncs: " ^ Cast.gen_c_prog pe.functions ^ "\n\nEverything: " ^
	Cast.string_of_program pe.all_statements)
