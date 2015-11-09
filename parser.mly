%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF IFELSE FOR WHILE INT CHAR BOOL DOUBLE VOID
%token <string> LITERAL
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
	LPAREN expr RPAREN 				{$2}
|	LPAREN ID expr RPAREN				{FunCall($2, $3)}
|	LPAREN WHILE expr  expr  RPAREN			{While($3, $4)}
|	LITERAL						{Lit($1)}
|	LPAREN INT ID expr RPAREN			{IntVarDec($3, $4)}
|	ID EQ ID					{Eq($1, $3)}
|	LPAREN ASSIGN ID expr RPAREN			{Assn($3, $4)}
|	expr expr					{Seq($1, $2)}
