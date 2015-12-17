open Sast

type prog_els = { variables: Cast.stmt list;
	functions: Cast.stmt list;
	all_statements: Cast.stmt list; }

(* turns S-ast to C-ast*)
let c_vtype = function s_vt -> (match s_vt with
	Sast.Int -> Cast.Int
|	Sast.Char -> Cast.Char
|	Sast.Double -> Cast.Double
|	Sast.Bool -> Cast.Bool
|	Sast.String -> Cast.String
|	Sast.Any -> Cast.Any)
let transform prog =
	let rec expr = function
	Tree(e,l) -> let l = List.map (fun e -> expr e) l in Cast.Tree(expr e,l)
	| IntLit(s) -> Cast.IntLit(s)
	| ChrLit(s) -> Cast.ChrLit(s)
	| FltLit(s) -> Cast.FltLit(s)
	| StrLit(s) -> Cast.StrLit(s)
	| GetBranch(e1,e2) -> Cast.GetBranch(expr e1, expr e2)
	| GetWidth(e1) -> Cast.GetWidth(expr e1)
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
	| Mod(e1,e2) -> Cast.Mod(expr e1,expr e2)
	| Cast(t,e) -> Cast.Cast(c_vtype t, expr e)
	| Id(s) -> Cast.Id(s) in

	let rec stmt = function
	While(e,s,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.While(expr e, stmt s, l)
	| If(e,s,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.If(expr e, stmt s, l)
	| IfElse(e,s,s2,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.IfElse(expr e, stmt s, stmt s2, l)
	| FuncDec(s, vt, vn, seq,l) -> let l = List.map (fun e -> stmt e) l in
	Cast.FuncDec(s, (c_vtype vt), vn, stmt seq, l)
	| VarDec(t,s,e) -> Cast.VarDec((c_vtype t),s,expr e)
	| Assn(s,e) -> Cast.Assn(s,expr e)
	| Expr(e) -> Cast.Expr(expr e)
	| Return(e) -> Cast.Return(expr e)
	| Seq(l) -> let l = List.map (fun s -> stmt s) l in
	Cast.Seq(l) in

	(* Get all funcs and top-level vars *)
	let vars = Sast.get_vars_list prog in
	let funcs = Sast.get_funcs_list prog in {
	variables = List.map (fun v -> stmt v) vars;
	functions = List.map (fun f -> stmt f) funcs;
	all_statements = List.map	(fun s -> stmt s) prog; }

(* Use the string_of funcs in cast.ml to make C file *)
let execute_prog prog outfile = let pe = transform prog in
	Printf.fprintf outfile "%s"
	(
		Cast.headers ^ "\n" ^
		Stubs.ll_c ^ "\n" ^
		Stubs.tree_c ^ "\n" ^
		Cast.gen_c_funcs pe.functions ^
		Cast.string_of_program pe.all_statements)
