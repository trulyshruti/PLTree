open Ast

module StringMap = Map.Make(String)

type env = {
	functions: string StringMap.t;
	globals: (Sast.expr * Sast.vtype) StringMap.t;
	locals: (Sast.expr * Sast.vtype) StringMap.t;
	statements: string StringMap.t;
}

let print_maps env =
	let printtf = (fun key (expr,t) -> Printf.printf "%s -> " key;
		print_endline (Sast.string_of_expr expr ^ " of " ^ Sast.string_of_vtype t)) in
	let printvals = (fun key value -> printtf key value) in
	print_string "globals:";
	StringMap.iter printvals env.globals;
	print_string "\nlocals:";
	StringMap.iter printvals env.locals; print_string "\n"

let translate prog =
	let rec add_all m = function
    	[] -> m
    	| hd::tl -> add_all (StringMap.add hd "" m) tl in

	let builtins = add_all StringMap.empty ["print"] in
	
	let empty_env = {
		functions = builtins;
		globals = StringMap.empty;
		locals = StringMap.empty;
		statements = StringMap.empty } in

	(* If there is a key in both maps, keeps value from first arg *)
	let merge_maps =
		let f = (fun k xopt yopt -> match xopt, yopt with Some x, _ -> xopt
			| None, yo -> yopt ) in StringMap.merge f in

	(* environment -> Ast.expr -> (Sast.expr, Sast.vtype) *)
	let rec expr env = function
	Tree(e,l) -> let l = List.map (fun e -> let (e,_) = expr env e in e) l in
		let (e,t) = expr env e in Sast.Tree(e,l), t
	| IntLit(s) -> Sast.IntLit(s), Sast.Int
	| ChrLit(s) -> Sast.ChrLit(s), Sast.Char
	| FltLit(s) -> Sast.FltLit(s), Sast.Double
	| StrLit(s) -> Sast.StrLit(s), Sast.String
	| GetBranch(e1,e2) ->(match e1 with Tree(e,l) -> let (se2,st) = expr env e2 in
	 (match st with Sast.Int | Sast.Double -> let (se1,t) = expr env e in
	 	Sast.GetBranch(se1,se2), t
	 | _ -> raise(Failure("Can only access branches with a number"))) (* TODO maybe bool/char? *)
	 | _ -> raise(Failure("No branches to be gotten"))) (* TODO test this func *)
	| Void -> Sast.Void, Sast.Int (* TODO what type to return *)
	| FunCall(s,e) -> if StringMap.mem s env.functions then
	let (e,t) = expr env e in Sast.FunCall(s,e), t
	else raise(Failure(s ^ " does not exist or is not visible"))

	| Eq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Eq(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Neq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Neq(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Lt(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Lt(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Leq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Leq(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Gt(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Gt(e1,e2), Sast.Bool else raise (Failure("Different types"))
	| Geq(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then Sast.Geq(e1,e2), Sast.Bool else raise (Failure("Different types"))

	| Add(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then match t1 with Sast.Int | Sast.Double -> Sast.Add(e1,e2), t1
		| _ -> raise(Failure("Addition operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Minus(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then match t1 with Sast.Int | Sast.Double -> Sast.Minus(e1,e2), t1
		| _ -> raise(Failure("Subtraction operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Mul(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then match t1 with Sast.Int | Sast.Double -> Sast.Mul(e1,e2), t1
		| _ -> raise(Failure("Multiplication operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Div(e1, e2) -> let (e1,t1) = expr env e1 in let (e2,t2) = expr env e2 in
	if t1 = t2 then match t1 with Sast.Int | Sast.Double -> Sast.Div(e1,e2), t1
		| _ -> raise(Failure("Divison operands must be of type int or double"))
	else raise (Failure("Different types"))
	| Id(s) -> if StringMap.mem s env.locals then StringMap.find s env.locals
	else if StringMap.mem s env.globals then StringMap.find s env.globals
	else raise(Failure(s ^ " does not exist or is not visible")) in

	(* environment -> Ast.stmt -> (environment, Sast.stmt) *)
	let rec transform_stmt env = function
		While(e,seq) -> env, let (e,t) = expr env e in
		if t = Sast.Bool then let locs = merge_maps env.locals env.globals in
		let env = {env with globals=locs; locals=StringMap.empty} in
		let (_,s) = transform_stmt env seq in Sast.While(e,s)
		else raise(Failure("While predicates must be of type bool"))
	| FuncDec(s,seq) -> env, let locs = merge_maps env.locals env.globals in
                let env = {env with globals=locs; locals=StringMap.empty} in
                let (_,seq) = transform_stmt env seq in Sast.FuncDec(s,seq) (* TODO *)
	| VarDec(s,e) -> if StringMap.mem s env.locals then
	raise (Failure (s ^ " is already declared")) else let (r,t) = expr env e in
	let locs = StringMap.add s (r,t) env.locals in {env with locals=locs},
	Sast.VarDec(s,r)
	| Assn(s,e) -> if StringMap.mem s env.locals then
	let (eSast,tSast) = StringMap.find s env.locals in
	let (r,t) = expr env e in if tSast = t then
	let locs = StringMap.add s (r,t) env.locals in {env with locals=locs},
	Sast.Assn(s, r) else raise(Failure(s ^ " is defined as " ^
	Sast.string_of_vtype tSast ^ ", not " ^ Sast.string_of_vtype t))
	else raise (Failure(s ^ " has not been declared"))
	| Expr(e) -> env, let (e,_) = expr env e in Sast.Expr(e)
	| Seq(l) -> let (env,l) = map_stmts env l in env, Sast.Seq(List.rev l) and

	(* environment -> Ast.stmt list -> (environment, Sast.stmt list) *)
	(* Maps ast stmts to sast stmts, passing environment along *)
	map_stmts env stmts =
		List.fold_left (fun (env, m) stmt ->
			let (e,s) = transform_stmt env stmt in
			let mapped = s::m in e, mapped) (env, []) stmts in

	let (env, transformed) = map_stmts empty_env prog in

	print_maps env;

	List.rev transformed
