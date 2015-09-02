Programming Assignment 4: Type-Checker

The files were split into the following based on functionality:
	- AstNodes.hs: Contains all the data types for holding the program,
	  classes, methods, features, formals, expressions and identifiers of
the .cl-ast file
	- Unserialize.hs: Takes in the .cl-ast file and organizes the program,
	  classes, methods/features, formals, and expressions into a nested
array of datatypes listed in AstNodes.hs.
	- Serialize.hs: Contains all the printing methods for class map,
	  implementation map, parent map and annotated AST. The order is
accurate with the cool --type map and the specifications given in the PA4
instructions.
	- main.hs: Contains all the error check methods, builds the class map,
	  implementation map, parent map and annotated AST. The error check
methods were split by which map checks for the error. Before any maps were
built, any inheritance cycle, unknown inherits object and redefinition of
objects were tested for to avoid crashes on the creation of any of the maps.
The implementation map checked for any errors regarding lack of a main method
or main class, the return type, formal names, formal types, and illegal
overrides regarding type, name or formals. The class map checks made sure no
attributes were redefined, their types and the self name. The parent map
checked for the self class and inheritance of Int, String or Bool. These three
map checks simply took in their respective maps and returned maybe an error.
After stripping the parent map and implementation map, the expressions were
typed and error checked individually. A conforms and lub check method for
lists and two values were created to avoid repeating this for the dispatches
and oher expression checks. The type map was created as the errors were being
checked because creating and checking the annotated AST is rather cumbersome.
There was a typeCheckExpression for each expression and every possible error
for each expression was weeded out before passing it up the expression tree.
The let bind and case element required their own separate methods due to poor
type choices for them. Each of the typeCheckExpression had the object map,
implementation map, class map, parent map and the expression passed in to make
sure scope was accounted for.

Test cases:
	bad1.cl - This tests for assigning a string to an integer variable. 
	bad2.cl - This tests for the predicate of a while loop not being an
integer value. 
	bad3.cl - This tests for a cyclical inheritance with more than just
two classes.
	good.cl - This tests for inheriting from the IO class, has a let
statement with multiple ID:Type <- blah statements, a while loop, use of
methods and an if statement. There are multiple assign statements with one new
statement. There are two classes, one with two methods to check for
alphabetization. The class Foo has a feature and a series of methods to
type-check.
