/*Tokens*/

%token CLASS ELSE IF FI IN INHERITS ISVOID LEFT LOOP POOL THEN WHILE CASE ESAC NEW OF NOT TRUE FALSE AT TILDE PLUS MINUS TIMES DIVIDE LBRACE RBRACE LPAREN RPAREN SEMI COLON COMMA LARROW RARROW DOT LT LE EQUALS TYPE IDENTIFIER STRING INTEGER

/*Associativity and Precedence*/

%right IN
%right LARROW
%left NOT not
%nonassoc LE LT EQUALS
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID isvoid
%left TILDE tilde
%left AT
%left DOT 
%left RPAREN
%right LPAREN lparen

%start file

/*Context free grammars*/

%%

file
	: program EOF {return $1;}
	;

program
	: class SEMI
	| class SEMI program
	;

class
	: CLASS type INHERITS type LBRACE feature_s RBRACE
	| CLASS type LBRACE feature_s RBRACE 
	;

feature_s
	:
	| feature_p
	;

feature_p
	: feature SEMI
	| feature SEMI feature_p
	;

feature
	: identifier LPAREN formal_option RPAREN COLON type LBRACE expr RBRACE
	| identifier COLON type larrow_option
	;

larrow_option
	:
	| LARROW expr
	;

formal_option
	:
	| formal formal_s
	;

formal
	: identifier COLON type
	;

formal_s
	:
	| formal_p
	;

formal_p
	: COMMA formal
	| COMMA formal formal_p 
	;

expr	
	: identifier LARROW expr 
	| expr AT type DOT identifier LPAREN expr_option RPAREN
	| expr DOT identifier LPAREN expr_option RPAREN
	| identifier LPAREN expr_option RPAREN
	| if expr THEN expr ELSE expr FI
	| while expr LOOP expr POOL
	| lbrace expr_p RBRACE
	| let identifier COLON type larrow_option id_type_expr_s IN expr 
	| case expr OF id_type_expr_p ESAC
	| new type 
	| isvoid expr 
	| expr PLUS expr
	| expr MINUS expr
	| expr TIMES expr 
	| expr DIVIDE expr
	| tilde expr 
	| expr LT expr 
	| expr LE expr
	| expr EQUALS expr
	| not expr
	| lparen expr RPAREN
	| identifier
	| integer 
	| string 
	| true 
	| false 
	;

expr_option
	: 
	| expr expr_s 
	;

expr_s
	:
	| expr_s_p
	;

expr_s_p
	: COMMA expr
	| COMMA expr expr_s_p
	;

expr_p
	: expr SEMI 
	| expr SEMI expr_p 
	;

id_type_expr_s
	: 
	| id_type_expr_s_p {$$ = $1;}
	;

id_type_expr_s_p
	: COMMA identifier COLON type larrow_option
	| COMMA identifier COLON type larrow_option id_type_expr_s_p
	;

id_type_expr_p
	: identifier COLON type RARROW expr SEMI
	| identifier COLON type RARROW expr SEMI id_type_expr_p
	;

/*The following non-terminals are created for the purpose of tracking expression line numbers*/

type
	: TYPE
	;

identifier
	: IDENTIFIER
	;

integer
	: INTEGER 
	;

if
	: IF 
	;

while
	: WHILE
	;

lbrace
	: LBRACE 
	;

let
	: LET 
	;

case
	: CASE 
	;

new
	: NEW 
	;

isvoid
	: ISVOID
	;

tilde
	: TILDE 
	;

not
	: NOT 
	;

lparen
	: LPAREN
	;

string
	: STRING 
	;

true
	: TRUE 
	;

false
	: FALSE
	;
