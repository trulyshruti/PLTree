%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF IFELSE FOR WHILE INT CHAR BOOL DOUBLE VOID
%token <string> LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE




%start expr
%type < Ast.expr> expr

%%



expr:
	LPAREN ID LITERAL RPAREN	{FunCall($2, $3)}
|	LPAREN ID ID RPAREN		{FunCall($2, $3)}
|	STRING_LITERAL			{Str($1)}
|	INT_LITERAL			{Int($1)}
|	FLOAT_LITERAL			{Flt($1)}
|	expr expr	{Seq($1, $2)}
