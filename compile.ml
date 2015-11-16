open Ast

module StringMap = Map.Make(String)

type env = {
	functions: string StringMap.t;
	globals: (vtype * float) StringMap.t;
	locals: (vtype * float) StringMap.t;
	statements: string StringMap.t;
}

let print_map env =
	let printtf = (fun key (t,f) -> Printf.printf "%s -> " key;
		print_endline (Ast.string_of_vtype t ^ " - " ^ string_of_float f)) in
	let printvals = (fun key value -> printtf key value) in
	print_string "globals:";
	StringMap.iter printvals env.globals;
	print_string "\nlocals:";
	StringMap.iter printvals env.locals

let translate prog =
	let empty_env = {
		functions = StringMap.empty;
		globals = StringMap.empty;
		locals = StringMap.empty;
		statements = StringMap.empty } in

	let rec expr env = function
		Lit(l) -> 1.
	| FunCall(s,e) -> 1.
	| Eq(e1, e2) -> 1.
	| Lt(e1, e2) -> 1.
	| Add(e1, e2) -> 2.
	| Id(s) -> 1. in

	let rec transform_stmt env = function
		While(e,s) -> let globals = env.globals in {env with globals=globals}
	| VarDec(t,s,e) -> let locals = let r = expr env e in
	StringMap.add s (t, r) env.locals in {env with locals=locals}
	| Assn(s,e) -> let locals = let r = expr env e in
	if (StringMap.mem s env.locals)
		then let (t,_) = StringMap.find s env.locals in
		StringMap.add s (t, r) env.locals else
		if (StringMap.mem s env.globals)
		then let (t,_) = StringMap.find s env.globals in
		StringMap.add s (t, r) env.locals else
		raise (Failure ("undeclared or mistyped variable " ^ s)) in
		{env with locals=locals}
	| Expr(e) -> ignore(expr env e); env
	| Seq(l) -> List.fold_left (fun env stmt -> transform_stmt env stmt) env l in

	let transformed = List.fold_left
		(fun env stmt -> transform_stmt env stmt) empty_env prog in

	print_map transformed
