
import lex as lex
import sys

INT_MAX = 2147483647

####################
#                  #
# lexer definition #
#                  #
####################

# INITIAL state is the default state. It is implicitly defined
states = [ ('COMMENT', 'exclusive'), ('LCOMMENT', 'exclusive') ]

# these are identifiers reserved by cool
reserved_ids = [ 'case',
                 'class',
                 'else',
                 'esac',
                 'fi',
                 'if',
                 'in',
                 'inherits',
                 'isvoid',
                 'let',
                 'loop',
                 'new',
                 'not',
                 'of',
                 'pool',
                 'then',
                 'while' ]

# all literals (or symbols)
lits = [ 'at',
         'colon',
         'comma',
         'divide',
         'dot',
         'equals',
         'larrow',
         'lbrace',
         'le',
         'lparen',
         'lt',
         'minus',
         'plus',
         'rarrow',
         'rbrace',
         'rparen',
         'semi',
         'tilde',
         'times' ]

# types that require a lexeme ('meta-data')
require_meta = [ 'string',
                 'integer',
                 'type',
                 'identifier' ]

# boolean keywords
bools = ['true', 'false']

# tokens to process comments
comments = [ 'lcomm', 'rcomm', 'linecomm', 'foo' ]

# combining sublists
tokens = bools + comments + lits + reserved_ids + require_meta

# simple regular expressions parse the literals
t_at         = r'@'
t_colon      = r':'
t_comma      = r','
t_divide     = r'/'
t_equals     = r'='
t_dot        = r'\.'
t_larrow     = r'<-'
t_lbrace     = r'{'
t_le         = r'<='
t_lparen     = r'\('
t_lt         = r'<'
t_minus      = r'-'
t_plus       = r'\+'
t_rarrow     = r'=>'
t_rbrace     = r'}'
t_rparen     = r'\)'
t_semi       = r';'
t_tilde      = r'~'
t_times      = r'\*'

# ignore spaces, tabs, and other archaic stuff
t_ignore  = ' \t\f\v\r'

'''
 processes identifier tokens. Depending on the form of the identifier, 
 this could result in outputing a boolean, type, or reserved keyword 
'''
def t_identifier(t):
    r'([A-Z]|[a-z])\w*'
    if t.value.lower() in reserved_ids:
        t.type = t.value.lower()
    elif t.value[0].isupper():
        t.type = 'type'
    else:
        if t.value.lower() in bools:
            t.type = t.value.lower()
    return t

# processes strings and insures they are within the max length
def t_string(t):
    r'"([^\n\0"\\]|\\[^\n\0])*"'
    if len(t.value) > 1026:
        string_error(t)
    return t

# processes integers and insures they are less than the max size
def t_integer(t):
    r'\d+'
    t.value = int(t.value)
    if t.value > INT_MAX:
        integer_error(t)
    return t

'''
signifies the start of a line comment. when encountered, 
the program switches into the LCOMMENT parsing state
'''
def t_linecomm(t):
    r'--'
    t.lexer.code_start = t.lexer.lexpos
    t.lexer.begin('LCOMMENT')

'''
signifies the start of a multiline comment. When encountered in either
the INITIAL or COMMENT state, the program switches into the COMMENT state,
pushing the current state onto a state that manages the traversal of states. 
This is necessary due to the possible of comments being nested.
'''
def t_INITIAL_COMMENT_lcomm(t):
    r'\(\*'
    t.lexer.code_start = t.lexer.lexpos
    t.lexer.lexstatestack.append( t.lexer.lexstate )
    t.lexer.begin('COMMENT')


'''
when the lexer encounters the right comment enclosure, the lexer returns 
to the lexing state at the top of the state stack
'''
def t_COMMENT_rcomm(t):
    r'\*\)'
    t.lexer.begin( t.lexer.lexstatestack.pop() )

'''
discard comment characters that are not matched in both 
comment states
'''
def t_COMMENT_LCOMMENT_foo(t):
    r'.'

# counts lines for both states
def t_INITIAL_COMMENT_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

'''
 counts lines in LCOMMENT state. Also switches lexer back to 
 the INITIAL state (comments are single line)
'''
def t_LCOMMENT_newline(t):
    r'\n+'
    t.lexer.begin('INITIAL')
    t.lexer.lineno += len(t.value)

# invalid character error (character not matched)
def t_error(t):
    print "ERROR: %i: Lexer: invalid character: %s" % (t.lineno, t.value[0])
    sys.exit()

# eof error called when lexing ends in multiline comment
def eof_error(lexer):
    print "ERROR: %i: Lexer: EOF in (* comment *)" % (lexer.lineno)
    sys.exit()

# error called when string is over max size
def string_error(t):
    print "ERROR: %i: Lexer: string length ***OVER 9000***" % (lexer.lineno)
    sys.exit()

# error called when integer is larger than max value
def integer_error(t):
    print "ERROR: %i: Lexer: integer ***OVER 9000***: %i" % (t.lineno, t.value)
    sys.exit()

###################
#                 #
# Lexer Execution #
#                 #
###################

# turn off lexer warnings
lexer = lex.lex(errorlog=lex.NullLogger())

# read input from file
lexer.input( open(sys.argv[1], 'r').read() )

# generate list of tokens using the generated lexer
tokens = []
while True:
    t = lexer.token()
    if not t: break
    tokens.append(t)

#making sure we are not inside a comment
if len(lexer.lexstatestack) != 0 and lexer.lexstatestack[0] != 'LCOMMENT':
    eof_error(lexer)
else:
    #write output to file
    f = open(sys.argv[1]+'-lex',"w")
    for t in tokens:
        f.write( str(t.lineno) + "\n" + t.type + "\n")
        # if current type requires lexeme
        if t.type in require_meta:
            f.write( str(t.value).replace('"', '') + "\n" )
