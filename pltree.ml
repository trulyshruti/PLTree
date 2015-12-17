type action = Ast | Compile

let _ =
	if Array.length Sys.argv > 2 then
		let infile = Sys.argv.(1) in
		let outfile = Sys.argv.(2) in
		let tempfile = infile ^ ".tmp" in
		Imports.check_imports infile tempfile;
		let out = open_out outfile in 
		let lexbuf = Lexing.from_channel (open_in tempfile) in
		let program = Parser.program Scanner.token lexbuf in
		Execute.execute_prog (Compile.translate program) out
	else raise(Failure("Must provide input and output file"))
 (*
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_endline listing
  (* | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing *)
  | Compile -> Execute.execute_prog (Compile.translate program)
 *)
