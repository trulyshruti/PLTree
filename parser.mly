%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COL
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF IFELSE FOR WHILE INT CHAR BOOL DOUBLE STRING VOID
%token <string> INT_LITERAL
%token <string> CHAR_LITERAL
%token <string> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%right RBRACE
%left LBRACE

%right RPAREN
%left LPAREN

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type < Ast.program> program

%%

program: 
	stmt program 	{$1 :: $2}
|	EOF		{[]}

vtype:
	INT		{Int}
|	CHAR		{Char}
|	DOUBLE		{Double}
|	BOOL		{Bool}
|	STRING		{String}


stmt_list:
			{[]}
|	stmt stmt_list 	{$1 :: $2}

stmt:
	LPAREN WHILE LPAREN expr RPAREN stmt_list RPAREN 	{While($4, Seq($6))}
|	LPAREN vtype ID expr RPAREN				{VarDec($3, $4)}
|	LPAREN ASSIGN ID expr RPAREN				{Assn($3, $4)}
|	expr							{Expr($1)}
|	LPAREN stmt RPAREN					{$2}

expr_list:
				{[]}
|	expr expr_list		{$1 :: $2}

expr:
	LPAREN ID expr RPAREN				{FunCall($2, $3)}
|	LBRACE expr_list RBRACE				{Tree(IntLit("0"), $2)}
|	COL expr LBRACE expr_list RBRACE		{Tree($2, $4)}
|	INT_LITERAL					{Tree(IntLit($1), [])}
|	CHAR_LITERAL					{Tree(ChrLit($1), [])}
|	FLOAT_LITERAL					{Tree(FltLit($1), [])}
|	STRING_LITERAL					{Tree(StrLit($1), [])}
|	ID						{Id($1)}
|	expr EQ expr					{Eq($1, $3)}
|	expr LT expr					{Lt($1, $3)}
|	LPAREN expr PLUS expr RPAREN			{Add($2, $4)}
