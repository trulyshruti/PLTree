%{ open Iast %}

%token <string> OPEN
%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token EOF

%start program
%type < Iast.program> program

%%

program:
	import program 	{$1 :: $2}
|	EOF		{[]}

import:
	OPEN		 	{Open($1)}
|	CHAR_LITERAL		{Other($1)}
|	STRING_LITERAL		{String($1)}
