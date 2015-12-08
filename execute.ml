open Sast

let get_vars prog =
  let rec get_vars_list = function
    [] -> []
    | hd::tl -> match hd with VarDec(_,_) -> hd::get_vars_list tl
              | _ -> get_vars_list tl in
  get_vars_list prog
let print_vars vars = Cast.string_of_program vars

let get_funcs prog =
   (* let rec get_funcs_list = function
    [] -> []
    | hd::tl -> match hd with FuncDec(_,_) -> hd::get_funcs_list tl
              | _ -> get_funcs_list tl in
  get_vars_list *) prog
let print_funcs funcs = Cast.string_of_program funcs

(* TODO: make variables a VarDec list *)
type prog_els = { variables: Cast.stmt list; }

 let transform prog =
  let rec expr = function
    Tree(e,l) -> let l = List.map (fun e -> expr e) l in
    Cast.Tree(expr e,l)
    | IntLit(s) -> Cast.IntLit(s)
    | ChrLit(s) -> Cast.ChrLit(s)
    | FltLit(s) -> Cast.FltLit(s)
    | StrLit(s) -> Cast.StrLit(s)
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
    While(e,s) -> Cast.While(expr e, stmt s)
    | VarDec(s,e) -> Cast.VarDec(s,expr e)
    | Assn(s,e) -> Cast.Assn(s,expr e)
    | Expr(e) -> Cast.Expr(expr e)
    | Seq(l) -> let l = List.map (fun s -> stmt s) l in
      Cast.Seq(l) in
  let vars = get_vars prog in
  let funcs = get_funcs prog in {
    variables = List.map (fun v -> stmt v) vars; }

let execute_prog prog =
  let pe = transform prog in print_endline (print_vars pe.variables)
