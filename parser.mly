%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COL
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF IFELSE FOR WHILE INT CHAR BOOL DOUBLE STRING VOID
%token <string> INT_LITERAL
%token <string> CHAR_LITERAL
%token <string> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%left SEMI

%right COL

%left RPAREN
%right LPAREN


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
	WHILE LBRACK expr RBRACK stmt_list SEMI 	{While($3, Seq($5))}
|	vtype ID expr SEMI 				{VarDec($2, $3)}
|	ID ASSIGN expr SEMI				{Assn($1, $3)}
|	expr SEMI					{Expr($1)}
|	LPAREN stmt RPAREN				{$2}

expr_list:
				{[]}
|	expr expr_list		{$1 :: $2}

expr:
	ID COL expr					{FunCall($1, $3)}
|	LBRACK expr_list RBRACK				{Tree(Void, $2)}
|	LBRACE expr RBRACE LBRACK expr_list RBRACK		{Tree($2, $5)}
|	INT_LITERAL					{Tree(IntLit($1), [])}
|	CHAR_LITERAL					{Tree(ChrLit($1), [])}
|	FLOAT_LITERAL					{Tree(FltLit($1), [])}
|	STRING_LITERAL					{Tree(StrLit($1), [])}
|	ID						{Id($1)}
|	expr EQ expr					{Eq($1, $3)}
|	expr LT expr					{Lt($1, $3)}
|	expr PLUS expr					{Add($1, $3)}
| expr MINUS expr					{Minus($1, $3)}
| expr TIMES expr					{Mul($1, $3)}
| expr DIVIDE expr				{Div($1, $3)}
|	LPAREN expr RPAREN				{$2}
