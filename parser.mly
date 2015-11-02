%{ open Ast %}

%token <int> LITERAL




%start expr
%type < Ast.expr> expr

%%


expr:
	LITERAL	{Lit($1)}

