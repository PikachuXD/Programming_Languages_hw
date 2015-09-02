/* ################################################
  This file was included for reading puproses only
 ################################################ */


/* our grammar returns a list of class objects. Each class object is recursively populated 
   with all relevant data down to the terminals. At each step, we are essentially creating
   a new 'node' that is a member of the parent object. Keeping track of meta-data is much easier
   this way. 
*/


%token AT CASE CLASS COLON COMMA DIVIDE DOT ELSE EQUALS ESAC FALSE FI IDENTIFIER IF IN INHERITS INTEGER ISVOID LARROW LBRACE LE LET LOOP LPAREN LT MINUS NEW NOT OF PLUS POOL RARROW RBRACE RPAREN SEMI STRING THEN TILDE TIMES TRUE TYPE WHILE

/* operator precedence. This mostly comes from thecool syntax guide, 
	although we had to do quite a bit (a *really* long time) of "experimenting" to make jison
	happy
*/
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
%%

// top level rule. returns final list of class objects
file
	: program EOF
		{ return $1 }
	;
		
// defines relationships between classes
program
    : class SEMI
		{  $$ = [$1] }
	| class SEMI program 
		{{ 
		   $$ = $3 
		   $$.unshift($1) 
		}}
    ;


// definition of class
class
	: CLASS type INHERITS type lbrace feature_maybe RBRACE
		{{
			$$ = { name: $2,
			       superclass: $4,
				   features: $6 }
		}}
	| CLASS type lbrace feature_maybe RBRACE
		{{
			$$ = { name: $2,
			       superclass: null,
				   features: $4 }
		}}
	;

// in our grammar, we appended "_maybe" to a generating expression that could be null
feature_maybe
	: 
		{ $$ = [] }
	| feature_recurser
		{ $$ = $1 }
	;

// we appended "_recurser" to a generating expression that could repeat
feature_recurser
	: feature SEMI 
		{ $$ = [$1] }
	| feature SEMI feature_recurser
		{{ 
			$$ = $3
			$$.unshift($1)
		}}
	;

// definition of feature
feature
	: identifier lparen formal_maybe RPAREN COLON type lbrace exp RBRACE 

		{{
			$$ = { name: $1, 
				   formal_list: $3,
				   kind: 'method',
				   type: $6,
				   body: $8 }
		}}
	| identifier COLON type init_exp_maybe
		{{
			if ($4 == null)
				$$ = { name: $1,
					   type: $3,
					   kind: 'attribute_no_init' }
			else
				$$ = { name: $1,
					   type: $3,
					   kind: 'attribute_init',
					   init: $4 }
		}}
	;


formal_maybe
	: 
		{ $$ = [] }
	| formal formal_recurser_maybe
		{{
			if( $2 == null)
				$$ = [$1]
			else{ 
				$$ = $2
				$$.unshift($1)
			}
		}}
	;

// a recurser that may repeat 0 or 1 or more times
formal_recurser_maybe
	:	
		{ $$ = [] }
	| formal_recurser
		{ $$ = $1 }
	;

formal_recurser
	: COMMA formal
		{ $$ = [$2] }
	| COMMA formal formal_recurser
		{{
			$$ = $3
			$$.unshift($2)
		}} 
	;

// definition of formal
formal
	: identifier COLON type 
		{{
			$$ = { name: $1,
				   type: $3 }
		}}
	;

// initialization expression: '<- exp'
init_exp_maybe
	:
		{ $$ = null }
	| LARROW exp
		{ $$ = $2 }
	;

// expression definition... this is quite large
// for each expression, we set the line numner of the 
// expression to the first token in the expression
exp
	: identifier LARROW exp
		{{
			$$ = { name : 'assign',
				   lhs : $1,
				   rhs : $3,
				   line : $1.line }
		}}
	| exp AT type DOT identifier lparen exp_maybe RPAREN
		{{
			var argums = $7
			if( argums == null)
				argums = []
			$$ = { name: 'static_dispatch',
			       exp: $1,
				   type: $3,
				   method: $5,
				   args: argums,
				   line : $1.line }
		}}
	| exp DOT identifier lparen exp_maybe RPAREN
		{{
			var argums = $5
			if( argums == null)
				argums = []
			$$ = { name: 'dynamic_dispatch',
			       exp: $1,
				   method: $3,
				   args: argums,
				   line : $1.line }
		}}
	| identifier lparen exp_maybe RPAREN
		{{
			var argums = $3
			if( argums == null)
				argums = []
			$$ = { name: 'self_dispatch',
			       method: $1,
				   args: argums ,
				   line : $1.line }
		}}
	| if exp THEN exp ELSE exp FI
		{{
			$$ = { name: 'if',
			       pred: $2,
				   then: $4,
				   els: $6 ,
				   line : $1.line }
		}}
	| while exp LOOP exp POOL
		{{
			$$ = { name: 'while',
			       pred: $2,
				   body: $4 ,
				   line : $1.line }
		}}
	| lbrace exp_semi_recurser RBRACE 
		{{
			$$ = { name: 'block',
			       body: $2 ,
				   line : $1.line }
		}}
	// this is likely the most complex rule in the grammar. The non-terminal names follow 
	// the established convention
	| let identifier COLON type init_exp_maybe id_type_init_exp_recurser_maybe IN exp
		{{
			if ($5 == null)
				$6.unshift( { name: 'let_binding_no_init',
				             variable: $2,
							 type: $4 } )	
			else
				$6.unshift( { name: 'let_binding_init',
				             variable: $2,
							 type: $4,
							 val: $5 } )	

			$$ = { name: 'let',
	               binds: $6,
				   exp: $8 ,
				   line : $1.line }
		}}
	| case exp OF id_type_rarrow_recurser ESAC
		{{
			$$ = { name: 'case',
	   			   case: $2,
				   elems: $4 ,
				   line : $1.line }
		}}
	| new type
		{{
			$$ = { name : 'new',
	               class: $2 ,
				   line : $1.line }
		}}
	| isvoid exp
		{{
			$$ = { name : 'isvoid',
			       rhs: $2 ,
				   line : $1.line }
		}}
	| exp PLUS exp
		{{
			$$ = { name : 'plus',
			       lhs: $1,
				   rhs: $3,
				   line : $1.line }
		}}
	| exp MINUS exp
		{{
			$$ = { name : 'minus',
			       lhs: $1,
				   rhs: $3,
				   line : $1.line }
		}}
	| exp TIMES exp
		{{
			$$ = { name : 'times',
			       lhs: $1,
				   rhs: $3 ,
				   line : $1.line }
		}}
	| exp DIVIDE exp
		{{
			$$ = { name : 'divide',
			       lhs: $1,
				   rhs: $3 ,
				   line : $1.line }
		}}
	| tilde exp
		{{
			$$ = { name : 'negate',
				   rhs: $2,
				   line : $1.line }
		}}
	| exp LT exp
		{{
			$$ = { name : 'lt',
				   lhs: $1, 
				   rhs: $3 ,
				   line : $1.line }
		}}
	| exp LE exp
		{{
			$$ = { name : 'le',
				   lhs: $1,
				   rhs: $3, 
				   line : $1.line }
		}}
	| exp EQUALS exp
		{{
			$$ = { name : 'eq',
				   lhs: $1, 
				   rhs: $3 ,
				   line : $1.line }
		}}
	| not exp
		{{
			$$ = { name : 'not',
				   rhs: $2 ,
				   line : $1.line }
		}}
	| lparen exp RPAREN
		{{
			$$ = { name: 'parens',
			       body: $2 ,
				   line : $1.line }
		}}
	| identifier 
		{{
			$$ = { name: 'identifier',
			       val: $1.val ,
				   line : $1.line }
		}}
	| integer
		{{
			$$ = { name: 'integer',
			       val: $1.val ,
				   line : $1.line }
		}}
	| string
		{{
			$$ = { name:'string',
			       val: $1.val ,
				   line : $1.line }
		}}
	| true
		{{ 
			$$ = { name: 'true',
				   line : $1.line }
		}} 
	| false
		{{ $$ = { name: 'false',
				 line : $1.line }
		}}
	;
		
	
exp_maybe
	: 
		{ $$ == null }
	| exp exp_recurser_maybe
		{{
			if ($2 == null )
				$$ = [$1]	
			else{
				$$ = $2
				$$.unshift($1)
			}
		}}
	;

exp_recurser_maybe
	: 
		{ $$ = null }
	| exp_recurser
		{ $$ = $1 }
	;

exp_recurser
	: COMMA exp 
		{ $$ = [$2] }
	| COMMA exp exp_recurser
		{{
			$$ = $3
			$$.unshift($2)
		}}
	;

// recurser the handles expressions seperated by semicolons
exp_semi_recurser
	: exp SEMI
		{ $$ = [$1] }
	| exp SEMI exp_semi_recurser
		{{
			$$ = $3
			$$.unshift($1)
		}}
	;
				

id_type_init_exp_recurser_maybe
	: 
		{ $$ = [] }
	| id_type_init_exp_recurser
		{ $$ = $1 }
	;

// this follows the same pattern as the more trivial recursive non terminals
id_type_init_exp_recurser
	: COMMA identifier COLON type init_exp_maybe
		{{
			if ($5 == null){
				$$ = [ { name: 'let_binding_no_init',
				             variable: $2,
							 type: $4 } ]	
			}
			else{
				$$ = [ { name: 'let_binding_init',
				             variable: $2,
							 type: $4,
							 val: $5 } ]	
			}
		}}
	| COMMA identifier COLON type init_exp_maybe id_type_init_exp_recurser
		{{
			if ($5 == null){
				$$ = $6 
				$$.unshift( { name: 'let_binding_no_init',
				             variable: $2,
							 type: $4 } )
			}
			else{
				$$ = $6 
				$$.unshift(  { name: 'let_binding_init',
				             variable: $2,
							 type: $4,
							 val: $5 } )
			}
		}}
	;

id_type_rarrow_recurser
	: identifier COLON type RARROW exp SEMI
		{{
			$$ = [ { variable: $1,
			         type: $3, 
					 body: $5 } ]
		}}
	| identifier COLON type RARROW exp SEMI id_type_rarrow_recurser
		{{
			$$ = $7
			$$ = $$.unshift( { variable: $1,
							type: $3, 
							body: $5 } )
		}}
	;

/* in order to get the line numbers of the first terminals in each expression using yylineno,
	the terminals have to abstracted out like this. We couldnt think of a more elegant way to 
	do this unfortunately. 
*/

identifier
	: IDENTIFIER
		{{
			$$ = { val: yytext, 
			       line: yylineno } 
		}}
	;

type
	: TYPE
		{{ 
			$$ = { val: yytext,
			       line: yylineno }
		}}
	;

if
	: IF
		{ $$ = { line: yylineno } }
	;

while
	: WHILE 
		{ $$ = { line: yylineno } }
	;

let
	: LET
		{ $$ = { line: yylineno } }
	;

case
	: CASE
		{ $$ = { line: yylineno } }
	;

new
	: NEW
		{ $$ = { line: yylineno } }
	;

isvoid
	: ISVOID
		{ $$ = { line: yylineno } }
	;

tilde
	: TILDE
		{ $$ = { line: yylineno } }
	;

not
	: NOT
		{ $$ = { line: yylineno } }
	;

lparen
	: LPAREN
		{ $$ = { line: yylineno } }
	;

lbrace
	: LBRACE
		{ $$ = { line: yylineno } }
	;

integer
	: INTEGER
		{{
			$$ = { val: yytext, 
			       line: yylineno } 
		}}
	;

string
	: STRING
		{{
			$$ = { val: yytext,
			       line: yylineno } 
		}}
	;

true
	: TRUE
		{{
			$$ = { line: yylineno } 
		}}
	;

false
	: FALSE
		{ $$ = { line: yylineno } }
	;
