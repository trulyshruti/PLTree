open Sast

let get_vars prog =
  let rec get_vars_list = function
    [] -> []
    | hd::tl -> match hd with VarDec(_,_) -> hd::get_vars_list tl
              | _ -> get_vars_list tl in
  get_vars_list prog
let print_vars vars = string_of_program vars

let get_funcs prog =
   (* let rec get_funcs_list = function
    [] -> []
    | hd::tl -> match hd with FuncDec(_,_) -> hd::get_funcs_list tl
              | _ -> get_funcs_list tl in
  get_vars_list *) prog
let print_funcs funcs = string_of_program funcs

let execute_prog prog =
  let vars = get_vars prog in
  let funcs = get_funcs prog in
  ignore(print_endline ("Variables: " ^ print_vars vars));
  ignore(print_endline ("Funcs: " ^ print_funcs funcs));
