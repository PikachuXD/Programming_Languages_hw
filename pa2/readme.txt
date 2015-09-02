( this text is not hard wrapped )

We chose to implement our lexer in python. This choice was largely due to the superior documentation available.  In addition, "hacking" jison to *only* output a lexer seemed more trouble than it was worth.

The code itself is all contained in one file, main.py. While this was not optimal (it violates a lot of OO principles), the examples given in the PLY documentation were all as such -- we didn't see a reason to re-invent the wheel. Our tokens were defined by several sublists, each delineated by some inherent meaning of the tokens therein (reserved keywords, lexems-required, etc.). This delineation made processing tokens and outputting the appropriate lexemes much easier.

The tokening rules were split between trivial regular expressions and functions that applied further processing to the regular expressions. Most "literal" values ( at, lparen, larrow, etc.) required no extra processing, while the string, integer, and identifier tokens required more specific logic.

Two additional lexing modes were implemented to process comments. In both of these states, all characters were discarded. When processing multi-line comments, a stack was used to keep track of the lexing state. Single line comments did not use a stack (they cannot be nested), but they were terminated at the invocation of the function that processes new lines.

All errors were encapsulated in functions to make invocation from the lexer methods easier. Each error cleanly terminates the program.

Most of our test cases were very simple (less than five tokens). Running diff on large files isn't very efficient when trying to pin down a specific problem. We did compare the output of our lexer on our rosetta.cl files to check line numbers, though. 

In all, we were both surprised by the robustness of the PLY lexer/parser. Things largely "just worked", and python was as wonderfully easy to code in as always. That being said, these niceties did not detract from the difficulty of correctly specifying the regular expressions. We grew rather tired of the tedium.
