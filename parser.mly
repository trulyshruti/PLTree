%{ open Ast %}


%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COL
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN ARROW POUND
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE WHILE INT CHAR BOOL DOUBLE STRING VOID ANY
%token <string> INT_LITERAL
%token <string> CHAR_LITERAL
%token <string> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF



%left EQ NEQ
%left LT GT LEQ GEQ MOD
%left PLUS MINUS
%left TIMES DIVIDE

%right COL

%right POUND

%left ARROW


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
|	ANY		{Any}


stmt_list:
				{[]}
|	stmt stmt_list 	{$1 :: $2}

stmt:
	WHILE COL expr LBRACK stmt_list RBRACK	 	{While($3, Seq($5))}
|	IF COL expr LBRACK stmt_list RBRACK		{If($3, Seq($5))}
|	IF COL expr LBRACK stmt_list RBRACK ELSE LBRACK stmt_list RBRACK {IfElse($3, Seq($5), Seq($9))}
|	ID COL vtype ID LBRACK stmt_list RBRACK		{FuncDec($1, $3, $4, Seq($6))}
|	vtype ID expr SEMI 				{VarDec($1, $2, $3)}
|	ID ASSIGN expr SEMI				{Assn($1, $3)}
|	expr SEMI					{Expr($1)}
|	RETURN COL expr	SEMI				{Return($3)}
|	LPAREN stmt RPAREN				{$2}

expr_list:
				{[]}
|	expr expr_list		{$1 :: $2}

expr:
	ID COL expr					{FunCall($1, $3)}
|	LBRACK expr_list RBRACK				{Tree(Void, $2)}
|	LBRACE expr RBRACE LBRACK expr_list RBRACK	{Tree($2, $5)}
|	INT_LITERAL					{Tree(IntLit($1), [])}
|	CHAR_LITERAL					{Tree(ChrLit($1), [])}
|	FLOAT_LITERAL					{Tree(FltLit($1), [])}
|	STRING_LITERAL					{Tree(StrLit($1), [])}
|	expr ARROW expr					{GetBranch($1, $3)}
|	ID						{Id($1)}
|	expr EQ expr					{Eq($1, $3)}
|	expr NEQ expr					{Neq($1, $3)}
|	expr LT expr					{Lt($1, $3)}
| 	expr LEQ expr					{Leq($1, $3)}
| 	expr GT expr					{Gt($1, $3)}
| 	expr GEQ expr					{Geq($1, $3)}
|	expr PLUS expr					{Add($1, $3)}
| 	expr MINUS expr					{Minus($1, $3)}
| 	expr TIMES expr					{Mul($1, $3)}
| 	expr DIVIDE expr				{Div($1, $3)}
|	expr MOD expr					{Mod($1, $3)}
|	POUND expr					{GetWidth($2)}
|	LPAREN expr RPAREN				{$2}
