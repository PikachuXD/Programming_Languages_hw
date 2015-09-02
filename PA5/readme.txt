Programming Assignment 5: Interpreter

After taking in the type map as a list of strings, the class map, the
implementation map and the parent map were spliced from the type map as tuples
containing the respective map and the rest of the file as the second part of
the tuple. Expressions were processed using the process identifier as the base
to keep track of line numbers necessary for error reporting. The let binds and
case elements were processed separately, with the type-checker rules as a
reference. The rest of the expressions were again processed using the type
checker as a reference. The class map processing initially starts with
dropping the word "class_map" from the type map and extracting the number of
classes as an integer. The find attributes was used as a reference for getting
classes, so the class map processing is somewhat more elegant. The return
value is always a tuple of (class_map addon, rest). An actual Map was used to
model the class map, especially useful for the new statement. The
implementation map worked similarly to the class map in that the return was
always (Implementation Map addon, rest). Again, there was an
actual implementation map (needed for dispatches), and the processing of implementation maps worked
from formals to methods to the actual full classes. The parent map was the
most straightforward as after finding the number of class pairs, the pairs
were stored as a tuple, then added to an actual parent map. The parent map is
primarily useful for the case statement, where the least common ancestor needs
to be found.

After processing the three maps, the basic environment, stack, and store had
to be initialized for going through the main class. After extracting the list
of attributes from the class map, the locations are initialized to match said
fields (which is stored as a tuple). This is to initialize the self object,
which contains the fields of the self class. The environment map is set up to
link the variable name to a location. The store links the location to the
value to an extra store, which is often returned from any given evaluated
expression statement. The stack is finished for setting up the main object and
the self object, the environment and the store.

Afterwards, the main method's expression is evaluated. This is done with the
recursive pattern matching method eval_exp. Overall, this is done with the
Cool Reference Manual's operational semantics as a reference, because it is
10/10 best documentation ever. As a more complicated example, the new
statement requires the stack to be updated and checked for stack overflow.
Afterwards, the type is pattern matched with SELF_TYPE or any other T.
Afterwards, the attribute list is found in the new class, similar to the self
object initialization earlier. The new fields are initialized and stored onto
the location/environment map. The object environment is updated with location, variable name and value. The new store has to factor in any initialized field
expressions and then the stack is finished. The let expression was another
pretty complex expression. The let bindings had to be stored in a new
environment and new store. The let bindings could each either have no
initializing expression or some initializing expression. If the expression
existed, then the value simply had to be initialized for the location map.
Otherwise, the value could simply be initialized to the default values as
listed in the reference manual. The internal expressions had to be determined
using purely understanding of what the actual methods returned, remembering to have the return value (if it
existed) as the first value of the tuple and the store as the second value.
These are explained in the commentary of the actual code.

The case statement works similar to the let in the sense that the initial
class' value, e0, is evaluated first and the string from that is extracted
(especially useful since the parent map is just tuples of strings). The null
default case is handled right off the bat. The case elements class names are
then extracted from the case elements as strings. Then the common ancestor
function is called, which essentially recursively creates an inheritance tree,
which checks its membership with the list of classes in the case statement. If
there is a match, return string, else return an empty string. After the branch
is selected, the location and store are updated then the actual expression of
the branch is evaluated.

Every method is commented in code but that basic logic based on the
operational semantics is roughly how every method was worked out.

Test cases:

test1.cl
This was a test of shallow copy. This made sure that the use of the copy()
method did not manipulate values based on reference. It made sure that the
copy() object was its own entity. The order of getX and getY was manipulated
to make sure that the shallow copy worked as intended regardless of order of
calling operations.

test2.cl
This was a test of a second kind of stack overflow using new objects. Not only
was this useful in making sure new worked, but it was also handy in checking
if the stack worked. If the user so wanted, the user could also uncomment the
recursive dispatch version of the stack overflow to make sure that worked.

test3.cl
This test simultaneously tests for if statement correctness, let statement
local environment changes, and checks for tabs, newlines, and expressions
within the out_int. 

test4.cl
This test checks for inheritance and case statement. The dispatch using the
new would create an object Y, which would use the method from the inherited X.
Since Y is a subtype of X, the case should use the branch of Y, instead of the
branch of X. This additionally checks for dispatch's correctness. 
