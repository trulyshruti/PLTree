open Sast

let get_vars prog =
  let rec get_vars_list = function
    hd::tl -> if hd = hd then hd::get_vars_list tl else []
    | _ -> [] in
  get_vars_list prog
let print_vars vars = string_of_program vars

let get_funcs prog = prog
let print_funcs funcs = "funcs"

let execute_prog prog =
  let vars = get_vars prog in
  let funcs = get_funcs prog in
  ignore(print_endline ("Variables: " ^ print_vars vars));
  ignore(print_endline (print_funcs funcs));
