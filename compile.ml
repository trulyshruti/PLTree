open Ast

module StringMap = Map.Make(String)

type env = {
	functions: Sast.vtype StringMap.t;
	globals: (Sast.expr * Sast.vtype) StringMap.t;
	locals: (Sast.expr * Sast.vtype) StringMap.t;
	statements: string StringMap.t;
}

(* prints the variables of an environment. For debugging. Could print funcs *)
let print_maps env =
	let printtf = (fun key (expr,t) -> Printf.printf "%s -> " key;
		print_endline (Sast.string_of_expr expr ^ " of " ^ Sast.string_of_vtype t)) in
	let printvals = (fun key value -> printtf key value) in
	print_string "globals:";
	StringMap.iter printvals env.globals;
	print_string "\nlocals:";
	StringMap.iter printvals env.locals; print_string "\n"
let avt_to_svt = function
	Ast.Int -> Sast.Int
|	Ast.Double -> Sast.Double
|	Ast.String -> Sast.String
|	Ast.Char -> Sast.Char
|	Ast.Void -> Sast.Void
|	Ast.Any	-> Sast.Any
|	Ast.Bool -> Sast.Bool
let matching t1 t2 = t1 == t2 || t1 == Sast.Any || t2 == Sast.Any
let translate prog =
	let rec add_all m = function
		[] -> m
		| (name,vtype)::tl -> add_all (StringMap.add name vtype m) tl in
	let builtins = add_all StringMap.empty [("put_t",Sast.Any)] in

	let empty_env = {
		functions = builtins;
		globals = StringMap.empty;
		locals = StringMap.empty;
		statements = StringMap.empty } in

	(* If there is a key in both maps, keeps value from first arg *)
	let merge_maps =
		let f = (fun k xopt yopt -> match xopt, yopt with Some x, _ -> xopt
			| None, yo -> yopt ) in StringMap.merge f in

	(* Extracts vardecs from a list of Sast.stmts *)
	let rec get_vars_list = function
		Sast.Seq([]) -> []
		| Sast.Seq(hd::tl) -> (match hd with Sast.VarDec(_,_,_) ->
			let l = Sast.Seq(tl) in hd::get_vars_list l
			| _ -> get_vars_list(Sast.Seq(tl)))
		| _ -> [] in

	(* environment -> Ast.expr -> (Sast.expr, Sast.vtype) *)
	let rec expr env = function
	Tree(e,l) -> let l = List.map (fun e -> let (e,_) = expr env e in e) l in
		let (e,t) = expr env e in Sast.Tree(e,l), t
	| IntLit(s) -> Sast.IntLit(s), Sast.Int
	| ChrLit(s) -> Sast.ChrLit(s), Sast.Char
	| FltLit(s) -> Sast.FltLit(s), Sast.Double
	| StrLit(s) -> Sast.StrLit(s), Sast.String
	| GetBranch(e1,e2) ->
		let (se2,st) = expr env e2 in (match st with Sast.Int ->
			let (se1,t) = expr env e1 in Sast.GetBranch(se1,se2), t
		| _ -> raise(Failure("Can only access branches with an int")))
	| GetWidth(e1) ->
		let (se1, st) = expr env e1 in Sast.GetWidth(se1), st
	| Void -> Sast.Void, Sast.Void
	| FunCall(s,e) -> if StringMap.mem s env.functions then
	let vt = StringMap.find s env.functions in
	let (e,t) = expr env e in if (vt == Sast.Any || t == vt) then Sast.FunCall(s,e), Sast.Any
	else raise(Failure(s ^ " expects an argument of type " ^ 
		Sast.string_of_vtype vt ^ ", not " ^ Sast.string_of_vtype t))
	else raise(Failure(s ^ " does not exist or is not visible"))
	| Eq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Eq(e1,e2), Sast.Bool else
	let type_string = Sast.string_of_vtype t1 ^ " == " ^ Sast.string_of_vtype t2 in
	raise (Failure("Different types: " ^ type_string))
	| Neq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Neq(e1,e2), Sast.Bool else 
	let type_string = Sast.string_of_vtype t1 ^ " != " ^ Sast.string_of_vtype t2 in
	raise (Failure("Different types: " ^ type_string))
	| Lt(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Lt(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Leq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Leq(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Gt(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Gt(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Geq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then Sast.Geq(e1,e2), Sast.Bool else raise (Failure("Different types"))

	(* Allow arithmetic on chars? *)
	| Add(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then match t1 with Sast.Int | Sast.Any | Sast.Double -> Sast.Add(e1,e2), t1
		| _ -> raise(Failure("Addition operands must be of type int or double"))
	else let type_string = Sast.string_of_vtype t1 ^ " + " ^ Sast.string_of_vtype t2 in
	raise (Failure("Different types: " ^ type_string))
	| Minus(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then match t1 with Sast.Int | Sast.Any | Sast.Double -> Sast.Minus(e1,e2), t1
		| _ -> raise(Failure("Subtraction operands must be of type int or double"))
	else let type_string = Sast.string_of_vtype t1 ^ " - " ^ Sast.string_of_vtype t2 in
	raise (Failure("Different types: " ^ type_string))
	| Mul(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then match t1 with Sast.Int | Sast.Any | Sast.Double -> Sast.Mul(e1,e2), t1
		| _ -> raise(Failure("Multiplication operands must be of type int or double"))
	else let type_string = Sast.string_of_vtype t1 ^ " * " ^ Sast.string_of_vtype t2 in
	raise (Failure("Different types: " ^ type_string))
	| Div(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then match t1 with Sast.Int | Sast.Any | Sast.Double -> Sast.Div(e1,e2), t1
		| _ -> raise(Failure("Divison operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Mod(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if (matching t1 t2) then match t1 with Sast.Int | Sast.Any | Sast.Double -> Sast.Mod(e1,e2), t1
		| _ -> raise(Failure("Mod operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Cast(vt, e) -> let (e,_) = expr env e in let vt = avt_to_svt vt in
	Sast.Cast(vt, e), vt
	| Id(s) -> if StringMap.mem s env.locals then
		let (e1, e2) = StringMap.find s env.locals in (Sast.Id(s), e2)
	else if StringMap.mem s env.globals then
		let (e1, e2) = StringMap.find s env.globals in (Sast.Id(s), e2)
	else raise(Failure(s ^ " does not exist or is not visible")) in

	(* environment -> Ast.stmt -> (environment, Sast.stmt) *)
	let rec transform_stmt env = function
		(* Change Ast stuff to Sast and keep track of vars new to this scope *)
		While(e,seq) -> env, let (e,t) = expr env e in
		if t = Sast.Bool then let locs = merge_maps env.locals env.globals in
		let env = {env with globals=locs; locals=StringMap.empty} in
		let (_,s) = transform_stmt env seq in let vars = get_vars_list s in
		Sast.While(e,s,vars)
		else raise(Failure("While predicates must be of type bool"))
	| If(e,seq) -> env, let (e,t) = expr env e in
		if t = Sast.Bool then let locs = merge_maps env.locals env.globals in
		let env = {env with globals=locs; locals=StringMap.empty} in
		let (_,s) = transform_stmt env seq in let vars = get_vars_list s in
		Sast.If(e,s,vars)
		else raise(Failure("If predicates must be of type bool"))
	| IfElse(e,seq,seq2) -> env, let (e,t) = expr env e in
		if t = Sast.Bool then let locs = merge_maps env.locals env.globals in
		let env = {env with globals=locs; locals=StringMap.empty} in
		let (_,s) = transform_stmt env seq in let vars = get_vars_list s in
		let (_,s2) = transform_stmt env seq2 in let vars2 = get_vars_list s2 in
		let allvars = List.concat [vars ; vars2] in
		Sast.IfElse(e,s,s2,allvars)
		else raise(Failure("If predicates must be of type bool"))
	| FuncDec(s, vt, vn, seq) -> let sexp = (match vt with
						Int -> Sast.IntLit("0"), Sast.Int
					|	Char -> Sast.ChrLit("0"), Sast.Char
					|	Double -> Sast.FltLit("0.0"), Sast.Double
					|	String -> Sast.StrLit("0"), Sast.String
					|	Any -> Sast.IntLit("0"), Sast.Any
					|	Bool -> Sast.IntLit("0"), Sast.Any
					| Void -> Sast.Void, Sast.Void ) in
		let locs = StringMap.add vn sexp StringMap.empty in
		let svt (_, vt) = vt in
		let funcs = StringMap.add s (svt sexp) env.functions in
		let env = {env with globals=StringMap.empty; locals=locs; functions=funcs} in
		let (_,seq) = transform_stmt env seq in let vars = get_vars_list seq in
		{env with functions=funcs}, Sast.FuncDec(s, (svt sexp), vn, seq,vars) (* TODO *)
	| VarDec(vt,s,e) -> if StringMap.mem s env.locals then
	raise (Failure (s ^ " is already declared")) else let (r,t) = expr env e in
	let locs = StringMap.add s (r,avt_to_svt vt) env.locals in {env with locals=locs},
	Sast.VarDec(avt_to_svt vt,s,r)
	| Assn(s,e) -> let (eSast,tSast) = if StringMap.mem s env.locals then
	StringMap.find s env.locals else if StringMap.mem s env.globals then
	StringMap.find s env.globals else raise (Failure(s ^
		" has not been declared"))
	in let (r,t) = expr env e in if tSast = t then
	let locs = StringMap.add s (r,t) env.locals in
	{env with locals=locs}, Sast.Assn(s, r) else
	raise(Failure(s ^ " is defined as " ^ Sast.string_of_vtype tSast ^
		", not " ^ Sast.string_of_vtype t))

	| Expr(e) -> env, let (e,_) = expr env e in Sast.Expr(e)
	| Return(e) -> env, let (e,_) = expr env e in Sast.Return(e)
	| Seq(l) -> let (env,l) = map_stmts env l in env, Sast.Seq(List.rev l) and

	(* environment -> Ast.stmt list -> (environment, Sast.stmt list) *)
	(* Maps ast stmts to sast stmts, passing environment along *)
	map_stmts env stmts =
		List.fold_left (fun (env, m) stmt ->
			let (e,s) = transform_stmt env stmt in
			let mapped = s::m in e, mapped) (env, []) stmts in

	let (env, transformed) = map_stmts empty_env prog in

	(* print_maps env; *)

	List.rev transformed
