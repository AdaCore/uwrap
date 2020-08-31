Templates
=========

Overall Structure
-----------------

A template is a structure that can be created through the wrapping process. It 
contains a set of fields that can be valued during template instantiation 
process. It structure is:

.. code-block:: text

   template <some name> <command>

or if the template is inheriting from a parent:

.. code-block:: text

   template <some name> extends <some parent> <command>

Field Types
-----------

TODO: Not all of the below fields types are implemented yet

Fields can be declared of the following types:

- string: this field is a simple string.
- text: this field is a text reference. It can be combined with other text and
  strings to crate dynamic text structure (more on that on a dedicated section 
  below).
- regexp: this field is a regular expression reference
- object: this field is a reference to an object, for example a node.
- set (string|object): this field contains a set indexed by string.
- map (string, [string|text|object|map|set]): this field is a map indexed by 
  strings

Template Commands and Variables
-------------------------------

Template can contain arbitrary complex commands, in particular commands that
contains themselves other commands and variables. For example:

.. code-block:: text

   template T do
      var v1: text => "A";
      var v2: text;

      do
         var v3: text;
         
         pick origin wrap T2 (v1 & v2 & v3);
      end;
   end;

This command will be executed only once, at template instanciation. Through
this execution, any variable declared will be stored as field in the template, 
in the order of evaluation. In particular, in the above case, the template will 
have a variable v1 and v2.

Upon template instanciation, these values can be modified, using either 
positional or named notation. For example:

.. code-block:: text

   match some_node ()
   wrap T (v1 => "X", v3 => "Y");

   match some_node ()
   wrap T ("X", "Y");

The left reference `@` is set during these parameter expressions, so that
the previous value of the variable can be refered to. 

Parameters expressions are computed in the calling context. If the wrapper
is named upon instanciation, this name can be used to refer to the template
already created variables. For example:

.. code-block:: text

   match some_node ()
   wrap w: T (v1 => @ & "X", v3 => w.v1 & "Y");

The specific creation of these variables will go as follow:

- All variables for a given block are created
- All default values for these variables are evaluated
- If parameters have been provided for the variables

In the above example, this will translate in:

- create v1 and v2 (root block)
- initialize v1 to "A"
- apply v1 parameter expression, v1 now is "AX"
- create v3 (nested block)
- apply v3 parameter expression, v3 is now "AXY"

Once a template has been instantiated (through a ``wrap`` or a ``weave`` clause),
further calls will not execute the command, but instead update the variables,
e.g.:

.. code-block:: text

   match some_node () do
      wrap T (v1 => @ & "X1", v3 => v1 & "Y1");
   then
      weave T (v1 => @ & "X2", v3 => v1 & "Y2");
   end;

Template Predicates
-------------------

Templates offer three kind of predicates:
- type predicates, testing on the template type
- field predicates, tested on the template fields
- origin, which refers to the node that is wrapped by this template
- peer, which allows to browse other templates (TODO: to implement)
- wrapper, which allows to browse the wrappers of a given node 
- kind, which provides a text-based version of the predicate type

Type predicates for templates work like other type perdicates. They are
is-predicates and can check if a template is of a given type. They allow for 
nested match expressions. For example:

.. code-block:: text

   template A_Template do

   end;

   match A_Template ()

TODO: for now, templates convert to empty string, work on that.

Field predicate allow to match against specific fields of a template. They 
behave like other field predicates, as is-predicates without parenthesis and 
has-predicate with parenthesis. There is no specific notation for the fields, 
as opposed to the nodes coming from langkit. For example:

.. code-block:: text

   template A_Template do
      var d1 : text;
      var d2 : text;
   end;

   match A_Template (d1 ("something)) and d2 ("something else");

   match d1 ("something");

``origin`` is a reference to the object that the template is wrapping. It can
be used like fields or other references, for example:

.. code-block:: text

   template A_Template do

   end;

   match DefiningName ()
   wrap A_Template ();

   match A_Template (origin (DefiningName ()));

``wrapper`` works on any node, and allows to browsed wrappers that have
been used to wrap this node so far. For example:

.. code-block:: text

   template A_Template do

   end;

   match BaseDecl ()
   wrap A_Template ();

   match parent (wrapper (A_Template));

TODO: peer needs to be implemented

``peer`` iterates over the templates of the same origin. It is a shortcut
to origin (template ()).

Inheritance
-----------

A template inheriting from another template will call its parent command before
its own. It will however be a distinct type - in particular a given node can be
wrapped with both a parent and its child templates, as well as different 
siblings. For example:

.. code-block:: text

   template A do
      V : text;
   end;

   template B extends A do
      V2 : text;
   end;

   match some_condition do
      wrap A;
      wrap B;
   end;

When matching on template predicate will match if a node is of the type or a 
child of the type of the predicate. For example:

.. code-block:: text

   match A () # will match for instances of A and B

Text Reference Evaluation
-------------------------

There is a fundamental difference to understand between text and string 
fields. String is a direct value. When referenced in an expression, its value
is evaluated directly and cannot change over time. Text is a text reference. 
When referenced in an expression, a pointer to that text is created. This 
pointer will only be resolved upon evaluation of the actual string.

For example:

.. code-block:: text

   template A do
      V1 : text;
      V2 : text;
      V3 : text;
   end;

   match some_other_predicate
   wrap a: A (
      V1 => "A", 
      V2 => "B",
      V3 => a.V1 & "-" & a.V2);

In the above code, V3 is a text structure, which has a reference to a.V1, 
a string "-", and a.V2. Its actual value will evolve as V1 and V2 will evolve. 
For example, a further iteration on the wrapper values could modify V1 and V2:

.. code-block:: text

   match A ()
   weave (
      V1 => @ & "_Weaved", 
      V2 => @ & "_Weaved");

As a consequence, the value of V3 which was initially "A-B" is now 
"A_Weaved-B_Weaved".

The value "A-B" could have been computed on the matcher, and then modified 
later on:

.. code-block:: text

   match V3 ("A-B")
   weave (
      V1 => @ & "_Weaved", 
      V2 => @ & "_Weaved");

Matching against V3 value doesn't fix its value, it just evaluates its current 
value which can be modified later one.

TODO: check that the following holds (it does for @ and should when referencing 
V1 directly too.)

Note that modifying V1 and V2 after themselves in the expression above doesn'this
create an infinite recursion. The reference to V1 is replaced upon 
``V1 => @ & "_Weaved" by a new reference that concatenates the old reference and
the string "_Weaved". Consequently, that old reference may itself still evolve
over time if it was build after references to other text fields.

The ``string ()`` conversion allows to force conversion of a text reference to
a final string, for example:

.. code-block:: text

  wrap a: A (
      V1 => "A", 
      V2 => "B",
      V3 => string (a.V1 & "-" & a.V2));

In the above example, V3 receives a raw string "A-B" which will not change 
anymore (even if it's referenced in a text).

Note that standard templates ``out`` and ``file`` evaluate at the very end of 
the program execution, so they will operate on text references after all links
have been made.

This capability is fundamental to the creation of complex wrapping texts, where
the warious wrapping and weavings steps are building a text structure from 
various places without a constraint order, and get resolved at the end of the
process.

Creation through new Functions
------------------------------

In some cases, node creation through the wrapping is not enough, and allocation
needs to be performed outside of the wrapped / wrapping system. This can be
created through the new () function. 

new () can be invoked in match and pick clauses. In a match clauses, it's always
return a reference to the object evaluated (and is therefore evaluated to true).
In a pick clause, the value returned by new will then be the target of the 
controlled actions.

In its simplest form, a new () operator contains a template instantiation 
expression similar to the one of a wrap and weave clause, which value can be 
captured. The only difference is that no origin is set, and therefore no peer
exist. For example:

.. code-block:: text

   template A do
      var V : text;
   end;

   match new (A ("some text"));

   pick new (A ("some text));

# TODO: only child is implemented for the model below. Implement others.

Templates can be created and inserted in an existing tree structure. To do
so, they need to be created within a tree browsing match nested expression. 
For example:

.. code-block:: text

   # will create a new child of type A
   match child (new (A ("some text")));
   
   # will insert a new sibling right after the current one
   pick next (new (A ("some text")));

   # will insert a parent between the current node and the current parent
   pick parent (new (A ("some text")));

Allocations can also happen in the context of boolean expressions. In this 
case, they will only be evaluated if needed to obtain a true result for the 
expression. If the boolean expression is a tree browsing match nested 
expression, then the new operator will only be evaluated if no other node 
matches the expression. For example:

.. code-block:: text

   # creates a new template instance A if the current node doesn't match A
   match "A" or new A ("A");

   # creates a new child "A" if no child matches "A".
   match child ("A" or new ("A"));

Allocator new also allows to create an tree structure at once, with curly 
brackets. Comma separated elements belong to the same level, each of them 
being optionally followed by a curly bracked pair to describe its children. 
For example:
 
#TODO: on the first example, which value is being picked? all 3 root ones
presumably? In a match, which value is returned? the last one?

.. code-block:: text

   # creates three siblings "A", "B" and "C"
   pick new ({A ("A"), A ("B"), A ("C")}).

   # creates a root "A" and two children "CHILD A" and "CHILD B", with 
   # "CHILD B" captured on x. 
   pick new (A ("A") {A ("CHILD A"), x: A ("CHILD B")})

Tree Browsing Predicates for Templates
--------------------------------------

While entities wrapping a given node belong to two structure: their own
structure, in particular if they have been created by allocators, as well as 
the structure of their origin. The tree browsing predicates parent, child, next,
prev and sbling will iterate over these two dimentions.

The rule is that the relation between two templates is the same as the one of 
the nodes they are wrapping. In other words, if A1 and B1 wrap N1, A2 and B2
wrap N2, and if N1 is the parent of N2, then A1 and B1 are also the parents of
A2 and B2.

Tree browsing predicates always browse the wrapping structure. Going to the 
wrapped nodes can only be done through a reference with ``origin``.

For example, let's consider a situation in Ada where a PackageDecl is a parent
of a SubpDecl, which we need to wrap respectively by w_PackageDecl and 
w_SubpDecl:

.. code-block:: text

   template w_PackageDecl;
   template w_SubpDecl;

   match PackageDecl
   wrap w_PackageDecl ();

   match SubpDecl ()
   wrap w_SubpDecl ();

There is a parent / child relationship between the w_PackageDecl and w_SubpDecl
which can be retreived by the regular tree browsing predicates. For example, 
if I want to capture the w_PackageDecl parent of a w_SubpDecl, I can write:

.. code-block:: text

   match w_SubpDecl () and parent (p: w_PackageDecl ())

Template Registries
-------------------

Every created template will eventually be itself browed by the main program.
However, it's sometimes convenient to access to all templates of a given type
that have been created. Templates profile a find predicate that allows to 
iterate over its instances:

.. code-block:: text

   template A do
      var V : text;
   end;

   match something and x: A.find (V ("A"))

In the above example, the first instance of A that has a text "A" for V will
be returned. This can also be used with an extension suffix:

.. code-block:: text

   pick A.all (); #TODO to implement, right now the syntax needs to go through a filter, e.g. A.filter().all()
