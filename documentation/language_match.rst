Match Clause
============

Overall Structure
-----------------

A match clause is a section of a command that conditionalizes its execution to
a specific expression. It's written:

.. code-block:: text

   match <some expression>

It can stand on its own, followed directly by a semicolon:

.. code-block:: text

   match <some expression>;

In this case, no action is applied, but the match clause can still have side 
effects that are executed under the above syntax.

A match clause is an optional section of a command. If not present, actions of
a command are executed uncondionally.

Actions conditioned by a match clause can be a sequence, a pick clause, a wrap
clause or a weave clause.

A match operates a comparison with the current element under iteration. This 
element is available within the match as the standard ``self`` variable. The
following for example is always true:

.. code-block:: text

   # TODO: add a test for the following
   match self

Predicates can apply to the ``self`` object or to the environment. They can
also apply to a specific object by prefixing a reference with a dot notation.
For example:

.. code-block:: text

   match some_reference.some_predicate

Most predicate can allow nested match expression, which is an expression testing
the value of the predicate itself:

.. code-block:: text

   match some_predicate (<some expression further testing the predicate>)

For example, the predicate can be checking for the existance of a field, and the
subpredicate checking the form of that field. Within a match nested expression,
the value of ``self`` is modified to the value currently tested. For example,
outside of the ``some_predicate`` call above, ``self`` is the value of the 
node currently being iterated, but within the parenthesis, `self` changes to be
the value tested by the predicate (for example a field).

Some predicate allow for both parameters and a match nested expression. When
that's the case, the match nested expression is always the last parameter. For
clarity, it can also be explicitely named:

.. code-block:: text

   match some_predicate (match => <some expression>)

match clauses can be followed by an else section:

.. code-block:: text

   match some_predicate
   else <some command>

The command introduce by the else can take any of the form that a command can
take, and will only be evaluated if the match predicate is false. In particular,
it can start itself by a match clause:

.. code-block:: text

   match some_predicate <some actions>
   else match some_other_predicate <some actions>
   else match yet_anoter_predicate <some actions>

is-predicates and has-predicates
--------------------------------

Expressing in match clauses are composed of predicates. There are two main types
of predicates:

- predicate that compare to ``self``, known as is-predicates.
- predicates verify that a given expression can be computed under the ``self``
  element environment, known as has-predicates.

There is no syntactical difference between has-predicates and is-predicates. 
Generally speaking, predicates that look like calls, such as:

.. code-block:: text

   match some_predicates ()

tend to be has-predicates. They check that an expression exists / can be 
computed in the ``self`` enviroment. Conversedly, predicates that look like 
references such as:

.. code-block:: text

   match some_predicates

tend to be is-predicates. They check that an expression corresponds to the
``self`` object.

It is possible to force a predicate to act as an is-predicate or an 
has-predicate by explicitely converting them:

.. code-block:: text

   match is (some_predicates ())
   match has (some_predicates)

Capturing Predicates Values
---------------------------

A predicate can either be evaluated to true or false. When evaluated to true,
it will also hold the value of an object. The value that gets returned by a
given predicates depends of its specification. Value capture is done with a name
followed by a semicolon. For example:

.. code-block:: text

   match x: some_predicate

Once captured, the captured value can be used in the rest of the expression. 
For example:

.. code-block:: text

   match x: some_predicate and x.some_predicate_on_x

Captured values are valuated early so that they are available in nested 
predicate expressions. For example:

.. code-block:: text

   match x: some_predicate (x.some_other_predicate)

Under the above code, the expression within parenthesis is an expression further
testing the value returned by some_predicate. The value currently being tested
can be named to the captured expression and referenced within that test.

This value is a temporary valuation. If the predicate happens not to match,
it will be reversed to its previous value upon exiting the predicate.

Capturing can also be used to keep track of various values of the self reference
through match expressions. For example:

.. code-block:: text

   match outer_self: self and some_property (self.something and outer_self.something);

Boolean Expressions
-------------------

Predicates can be combined with boolean expressions. ``and``, ``or`` and 
``not`` operators are available. These operators are short-cutting operators, 
so that in:

.. code-block:: text

   match a or b

b is only evaluated if false is false. 

When the result is true, boolean expressions also return a value. This value
can be captured. ``and`` and ``or`` operators have less priority than value 
capture. For example:

.. code-block:: text

   match a: some_value and some_other_value

the value a will receive the value of some_value if some_value is true. This
allows to capture all the values of a boolean expression:

.. code-block:: text

   match a: some_value and b: some_other_value

The value of the entire expression can be captured through parenthesis:

.. code-block:: text

   match v1: (a or b)
   match v2: (c and d)

In that case, the value captured is the value of the last operand, so that
in the above example, v1 is a if a is true, b if a is false and b is true. v2
is always d if c and d are true. The not boolean expression will valuate to 
``self`` if returning true. For example

.. code-block:: text

   # TODO: this probably works, but to test
   match x: (not a)

.. code-block:: text

   match a: some_value and b

TODO We need to implement and document the behavior of value capture if only part
of the expression matches. For example:

.. code-block:: text

   match a: some_value and b: some_other_value

if some_value is true and some_other_value is false, then a and b are not valuated.

Strings and Regular Expression Predicates
-----------------------------------------

Objects under iteration can always convert to strings. For example, in Ada, 
that string is the textual content of the node. This string can be matched 
against a regular expression. In UWrap, literal strings are always interpreted 
as regular expressions, and are is-predicates. For example:

.. code-block:: text

   match "ABC"

checks that the string "ABC" is contained in the text of the current node. The
string:

.. code-block:: text

   match "^ABC$"

Checks that the text is exactly ABC.

The full documentation for the regular expression language is decribed in the
GNAT.Regpat package of the GNAT Compiler (TODO - Add reference).

Values within Strings can be captured, either by number of by name, with the
name identified by the "?<some name>" syntax following the open parenthesis of
the capture group:

.. code-block:: text

   match "^(.*)-(.*)$"
   match "^(?<prefix>.*)-(?<suffix>.*)$"

As soon as valuated, values can be used in further subexpressions. Note that
only named groups can be directly referenced:

.. code-block:: text

   match "^(?<prefix>.*)-(?<suffix>.*)$" and prefix ("A.*B")

String can be built by evaluating expressions. Groups captured by numbers can
be referenced by "\number" syntax, with numbers starting at 1. For example:

.. code-block:: text

   # checks that the suffix is of the form A followed by prefix followed by B,
   # e.g. Something-[Something]
   match "^(.*)-(?<suffix>.*)$" and suffix ("[\1]")

Arbitrary expressions can be introduced by the "\e" escapement character,
followed by the expression surrounded by "<>". For example:

.. code-block:: text

   match "^(?<prefix>.*)-(?<suffix>.*)$" and suffix ("[\e<prefix>]")

Type Predicates
---------------

Nodes under iteration are associated with predicates that allow to check for
their type. These predicates operate as is-predicates. For example, with the
ada language:

.. code-block:: text

   # TODO the below expression doesn't currently work, to fix and test
   match DefiningName

the predicate will evaluate to true if the current node is of type DefiningName.
A type matcher can also accomodate a nested expression:

.. code-block:: text

   match DefiningName (a or b)

In this case, the predicate will be true if the self node is of kind 
DefiningName and ``a or b`` is true. The above is equivalent to:

.. code-block:: text

   match DefiningName and (a or b)

The value returned by a type predicate when true is the value of the object 
currently iterated on. So that:

.. code-block:: text

   match v1: DefiningName
   match v2: DefiningName (a or b)

both value v1 and v2 to ``self`` if the predicate is true. 

When nodes types are themselves hierarchical, type predicate will value to true
if the node type hierarchy includes that type. For example, in Ada, on a 
subprogram declaration:

.. code-block:: text

   match BasicDecl
   match SubpDecl

both matchers will resolve to true.

Fields Predicates
-----------------

Nodes under iteration can declare fields in various ways. Nodes coming from 
langkit such as Ada nodes declare all fields with the f_ prefix. Fields 
predicates act as is-predicates when they're directly reference, as in:

.. code-block:: text

   match f_something

Meaning "check that the ``self`` element correspond to f_something. 

They act as has-predicate when providing a nested expression, as in:

.. code-block:: text

   match f_something ()
   match f_something (a or b)

Meaning "check that the ``self`` element has a field named f_something that is
of a given form.

The value returned by a field predicate is the value of that field, so that:

.. code-block:: text

   match f: f_something

f has the value of f_something if it exist.

Within a field predicate, the value of self is switched the value of that 
field. For example:

.. code-block:: text

   match f_something (DefiningName ("ABC"))

checks that the node under iteration has a field called f_something, which is
of type DefiningName and checks the regular expression "ABC". 

Properties and Functions Predicates
-----------------------------------

Properties and function are similar to field predicates, except for the fact 
that they always needs parenthesis to be invoked, and may have parameters.
Properties predicates provided by lankit-based nodes, in particular Ada nodes, 
are prefixed by p_.

Properties and function return a value that can be matched with a nested 
matching expression, and captured through a capture expression. For example:

.. code-block:: text

   match l: to_lower (self)

the above capture the lower case of self.

.. code-block:: text

   match to_lower (self, "abc")
   match to_lower (self, match => "def")

The above check that lowercased self match abc or def.

Tree Browsing Predicates
------------------------

Nodes surrounding the current node can be tested through a number of predicates
testing its structure:

- parent (<match expression>) is true if any parent matches the expression
- child (<match expression>) is true if any child matches the expression
- next (<match expression>) is true if a node a the same level after the current
  node matches the expression
- prev (<match expression>) is true if a node at the same level before the 
  current node matches the expression
- sibling (<match expression) is true if an node at the same level before or 
  after the current node maches the expression

The expression is optional, so that:

.. code-block:: text

   match prev ()

only matches if there is a node before the current one.

Within the matching nested expression, ``self`` take the value of the node
currently being tested. The expression will be tested for all value that can
be browed up until one matches, and will then returned this value that can be
captured. For example:

.. code-block:: text

   match c: child (DefiningName ("BLA"))

will check within all children of the current node for one of type DefiningName
that contains the text "BLA", and return the first occurence found. Capturing
the value can also be done within the nested expression:

.. code-block:: text

   match child (c: DefiningName ("BLA"))

Tree browsing predicates can be combined with boolean expressions or nested
expressions. For example:

.. code-block:: text

   match child (next (DefiningName ("A"))) and prev ("B")

the above checks for a node that has a child with a next node containing "A", 
and that also has a previous node called "B".

Pattern Sequence Expressions
----------------------------

TODO: the description below is to be implemented and may vary

Tree browsing predicates can check for a sequence of nodes instead of a unique
node. Elements of this sequence are separated by \. For example:

.. code-block:: text

   match child ("A" \ "B")

checks for a node containing the text "A" directly followed by a node containing
the text "B". When \ is place at the begining of the sequence, it anchors to the
first element tested, \ at the end anchors to the last. E.g.:

.. code-block:: text

   match child (\ "A" \ "B" \)

Match for a node that has a child sequence with one direct child "A" and one 
direct grandchild "B" with no more children.

The predicates ``many`` or ``few`` allow to match for "as many as possible" or 
"as few as possible" elements and correspond to the usual greedy and lazy 
quantifiers operators. By default, they match 0 to any number of elements. They
can accept a parameter min and a parameter max. For example:

.. code-block:: text

   match child (\ "A" \ many (true) \ "B" \)

The above matches for a sequence of children where the first is "A", then 
accepts as many nodes as possible then expects a "B".

There's no optional operator available - instead ``many`` and ``few`` can
be used with proper min and max values, for example:

.. code-block:: text

   match child (\ "A" \ many (true, 0, 1) \ "B" \)

Note that child predicate isn't meant to describe the entire descendance of
a node directly - it checks for the existence of at least one chain of 
descendants matching a given pattern.

The ``self`` value is modified in each subset of the sequence, and takes the
value of the currently analyzed node. It can be captured. The result of a 
sequence is the last element being matched. For example:

.. code-block:: text

   match r: child (\ "A" \ last: many (true) \ "B" \)

In the above, is matched, r is the value of the grandchild. last is the value
of the last element being matched by the many predicate.
