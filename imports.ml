open Printf

let rec lex_file filename outfile =
  	let lexbuf = Lexing.from_channel (open_in filename) in
	let program = Iparser.program Iscanner.token lexbuf in
	let rec print_program = 
		function outfile ->
		function
		Iast.Open(s)::tl -> lex_file s outfile; print_program outfile tl
	|	Iast.Other(c)::tl -> fprintf outfile "%c" c; print_program outfile tl
	|	Iast.String(s)::tl -> fprintf outfile "%s" ("\"" ^ s ^ "\""); print_program outfile tl
	|	[] -> () in
	print_program outfile program
	

let check_imports infilename outfilename = 
	let out = open_out outfilename in
	lex_file infilename out; close_out out
