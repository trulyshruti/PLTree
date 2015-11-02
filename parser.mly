%{ open Ast %}

%token <string>  LITERAL
%token <int> 	 INT
%token EOF





%start expr
%type < Ast.expr> expr

%%


expr:
	LITERAL		{Lit($1)}
|	INT 		{Int($1)}
|	expr expr	{Seq($1, $2)}
