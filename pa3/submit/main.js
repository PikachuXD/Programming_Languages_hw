/************************************
 *  Definition of Async File input  *
 ************************************/

// hold the token objects from input
var cl_lex = []

// specifies which tokens require a lexeme
var req_lexeme = ['string', 'integer', 'type', 'identifier'] 

//opening input file
fs = require('fs')
fs.readFile(process.argv[2], 'utf8', function (err,data) {

	if (err) {
		return console.log("ERROR: Parser: Could not open lex file.");
	}

	/* this single line was the source 
	 * of infinite sorrow
	 * ...a carriage return? really!?
	 */
	var lines = data.split(/\r\n|\n/);
		
	// looping through split lines and building token objects
	var i = 0;
	var count = 0;
	while ( i < lines.length-1 ){
		
		var token = { pos : Number(lines[i]), id : lines[i+1] }

		if ( req_lexeme.indexOf(token.id) >= 0 ){
			token.lexeme = lines[i+2]	
			i += 3
		}
		else
			i += 2
		
		cl_lex[count++] = token
	}

	/*********************************
	 *  Executing the parser         *
	 *********************************/
	//calling parser
	var final_ast = parser.parse("")
	//creating output file
	var outFile = process.argv[2].substring(0, process.argv[2].length-3) + "ast"
	var out = fs.createWriteStream(outFile, {encoding: 'utf8'})
	//printing final tree to output file
	printAST( final_ast, out)

});

/*********************************
 *  Definition of (Hacked) Lexer *
 *********************************/

var parser = require("./gram.js").parser;

// overriding the default lexer. This is so 
// hacky its ridiculous 
var token_count = 0
parser.lexer = {
	lex : function() {
		if ( token_count == cl_lex.length )
			return "EOF"
		var token = cl_lex[token_count++]
		this.yytext = token.lexeme
		this.yylineno = token.pos
		return token.id.toUpperCase()
	},
	setInput : function(str) { },
}

// overriding the default error function
parser.yy.parseError = function(str, hash){
	console.log("ERROR: " + hash.line + ": Parser: syntax error near " + hash.token)
	process.exit(1)
}


/*********************************
 *   printing function           *
 *********************************/

// top - level function that prints the entire returned ast object to given file 
// instance
function printAST(ast, out){

	out.write(ast.length + "\n")
	// printing classes
	ast.forEach( function(cls) {
		printID(cls.name, out)

		if(cls.superclass == null)
			out.write('no_inherits\n')	
		else{
			out.write('inherits\n')
			printID(cls.superclass, out)
		}
		
		// printing features
		out.write(cls.features.length + '\n')
		cls.features.forEach( function(feat){
			out.write(feat.kind + '\n')
			printID(feat.name, out)

			if( feat.kind == 'attribute_init'){
				printID(feat.type, out)
				printExp(feat.init, out)
			}
			else if (feat.kind == 'attribute_no_init'){
				printID(feat.type, out)
			}
			else {
				out.write(feat.formal_list.length + '\n')
				// printing formals
				feat.formal_list.forEach( function(form){
					printID(form.name, out)
					printID(form.type, out)
				})

				printID(feat.type, out)
				printExp(feat.body, out)
			}
		})
	})
}

// prints an identifier
function printID(id, out){
	out.write(id.line + "\n")
	out.write(id.val + "\n") 
}

// helper function to print expressions. We determine what to print using the names
// of the expression objects
function printExp(exp, out){

	//console.log(exp)
	if( exp.name == 'parens' ){
		printExp( exp.body, out )
		return;
	}
	
	out.write( exp.line + '\n' )
	out.write( exp.name + '\n' )

	switch ( exp.name ) {

		case 'assign':
			printID(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'static_dispatch':
			printExp(exp.exp, out)		
			printID(exp.type, out)
			printID(exp.method, out)
			out.write( exp.args.length + '\n')
			exp.args.forEach( function(arg){
				printExp(arg, out)	
			})
		break;

		case 'dynamic_dispatch':
			printExp(exp.exp, out)		
			printID(exp.method, out)
			//console.log(exp)
			out.write( exp.args.length + '\n')
			exp.args.forEach( function(arg){
				printExp(arg, out)	
			})
		break;

		case 'self_dispatch':
			printID(exp.method, out)
			out.write( exp.args.length + '\n')
			exp.args.forEach( function(arg){
				printExp(arg, out)	
			})
		break;

		case 'if':
			printExp(exp.pred, out) 
			printExp(exp.then, out) 
			printExp(exp.els, out) 
		break;

		case 'while':
			printExp(exp.pred, out)
			printExp(exp.body, out)
		break;

		case 'block':
			out.write(exp.body.length + '\n')	
			exp.body.forEach( function(it) {
				printExp(it, out)
			})
		break;

		case 'let':
			out.write( exp.binds.length + '\n')	
			exp.binds.forEach( function(bind){
				out.write(bind.name + '\n')
				printID( bind.variable, out)
				printID( bind.type, out)
				if (bind.hasOwnProperty('val'))
					printExp(bind.val, out)
			})
			printExp(exp.exp, out)
		break;

		case 'case':
			printExp( exp.case, out)
			out.write( exp.elems.length + '\n')
			exp.elems.forEach( function(elem){
				printID(elem.variable, out)
				printID(elem.type, out)
				printExp(elem.body, out)
			})
		break;

		case 'new':
			printID( exp.class, out)
		break;

		case 'isvoid':
			printExp(exp.rhs, out)
		break;

		case 'plus':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'minus':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'times':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'divide':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'negate':
			printExp(exp.rhs, out)
		break;

		case 'lt':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'le':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'eq':
			printExp(exp.lhs, out)
			printExp(exp.rhs, out)
		break;

		case 'not':
			printExp(exp.rhs, out)
		break;

		case 'identifier':
			printID( exp, out)
		break;
		
		case 'integer':
			out.write( exp.val + '\n')
		break;

		case 'string':
			out.write( exp.val + '\n')
		break;

		case 'true':
		break;
		
		case 'false':
		break;
	}
}
