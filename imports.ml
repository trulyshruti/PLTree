open Printf

let stdlibdir = "/usr/local/bin/pltree_std/"

let rec lex_file filename outfile opened_list =
  	let lexbuf = Lexing.from_channel (
		try open_in filename with 
			e -> open_in (Filename.concat stdlibdir filename)
	) in
	let program = Iparser.program Iscanner.token lexbuf in
	let rec print_program = 
		function outfile ->
		function l -> 
		function
		Iast.Open(s)::tl -> 	if List.exists (function a -> s = a) l then (
						print_program outfile l tl
					) else (	
						print_program outfile (lex_file s outfile (s::l)) tl
					)
	|	Iast.Other(c)::tl -> fprintf outfile "%c" c; print_program outfile l tl
	|	Iast.String(s)::tl -> fprintf outfile "%s" ("\"" ^ s ^ "\""); print_program outfile l tl
	|	[] -> l in
	print_program outfile opened_list program
	

let check_imports infilename outfilename = 
	let out = open_out outfilename in
	ignore (lex_file infilename out []); close_out out
