open Ast

let rec eval = function 
    Var(var, expr) -> let v1 = eval var and v2 = eval expr in
    	print_endline (v1 ^ v2)
  | Dec(func_dec, expr) -> let v1 = eval func_dec and v2 = eval expr in
    	print_endline (v1 ^ v2)
  | Call(func_call, expr) -> let v1 = eval func_call and v2 = eval expr in
    	print_endline (v1 ^ v2)
  | Asn(asn, expr) -> let v1 = eval asn and v2 = eval expr in
    	print_endline (v1 ^ v2)
  | Lit(literal) -> literal
  | Binop(e1, op, e2) ->
      let v1 = eval e1 and v2 = eval e2 in
      match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result = eval expr in
  print_endline (string_of_int result ^ " ")
  (* concatenate with space to remove ^D output *)
