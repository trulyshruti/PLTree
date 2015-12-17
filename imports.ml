open Printf


let rec lex_file filename outfile opened_list =
  	let lexbuf = Lexing.from_channel (open_in filename) in
	let program = Iparser.program Iscanner.token lexbuf in
	let rec print_program = 
		function outfile ->
		function l -> 
		function
		Iast.Open(s)::tl -> 	if List.exists (function a -> s = a) l then (
						print_program outfile l tl
					) else (
						lex_file s outfile (s::l); 
						print_program outfile (s::l) tl
					)
	|	Iast.Other(c)::tl -> fprintf outfile "%c" c; print_program outfile l tl
	|	Iast.String(s)::tl -> fprintf outfile "%s" ("\"" ^ s ^ "\""); print_program outfile l tl
	|	[] -> () in
	print_program outfile opened_list program
	

let check_imports infilename outfilename = 
	let out = open_out outfilename in
	lex_file infilename out []; close_out out
