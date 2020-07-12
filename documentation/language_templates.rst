Templates
=========

Overall Structure
-----------------

A template is a structure that can be created through the wrapping process. It 
contains a set of fields that can be valued during template instantiation 
process. It structure is:

.. code-block:: text

   template <some name> {
      #TODO implement the default value syntax
      var <field name> : <field type> => <default value>;
      <other fields>
   }

or if the template is inheriting from a parent:

.. code-block:: text

   template <some name> extends <some parent> {
      var <field name> : <field type>;
      <other fields>
   }

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

Template Predicates
-------------------

Templates offer three kind of predicates:
- type predicates, testing on the template type
- field predicates, tested on the template fields
- origin, which refers to the node that is wrapped by this template
- peer, which allows to browse other templates
- template, which allows to browse the templates of a given node (including and
  source node).

Type predicates for templates work like other type perdicates. They are
is-predicates and can check if a template is of a given type. They allow for 
nested match expressions. For example:

.. code-block:: text

   template A_Template {

   }

   match A_Template ()

TODO: for now, templates convert to empty string, work on that.

Field predicate allow to match against specific fields of a template. They 
behave like other field predicates, as is-predicates without parenthesis and 
has-predicate with parenthesis. There is no specific notation for the fields, 
as opposed to the nodes coming from langkit. For example:

.. code-block:: text

   template A_Template {
      var d1 : text;
      var d2 : text;
   }

   match A_Template (d1 ("something)) and d2 ("something else");

   match d1 ("something");

``origin`` is a reference to the object that the template is wrapping. It can
be used like fields or other references, for example:

.. code-block:: text

   template A_Template {

   }

   match DefiningName ()
   wrap A_Template ();

   match A_Template (origin (DefiningName ()));

``template`` works on any node, and allows to browsed the templates that have
been used to wrap this node so far. For example:

.. code-block:: text

   template A_Template {

   }

   match BaseDecl ()
   wrap A_Template ();

   match parent (template (A_Template));

TODO: peer needs to be implemented

``peer`` iterates over the templates of the same origin. It is a shortcut
to origin (template ()).

Inheritance
-----------

A template inheriting from another template will inherit from all its fields. 
It will however be a distinct type - in particular a given node can be wrapped
with both a parent and its child templates, as well as different siblings. For
example:

.. code-block:: text

   template A {
      V : text;
   }

   template B extends A {
      V2 : text;
   }

   match some_condition {
      wrap A;
      wrap B;
   }

When matching on template predicate will match if a node is of the type or a 
child of the type of the predicate. For example:

.. code-block:: text

   match A () # will match for instances of A and B

Creation through Wrap and Weave Clauses
---------------------------------------

Nodes can be wrapped with templates through wrap and weave clauses. These 
clauses can valuate one of several of the templates fields, either by position
or through a named notation. Named notation doesn't require names to be in 
order and can be introduced after a positional notation. Positional notation
however cannot be re-introduced after switching to name notation. For example:

.. code-block:: text

   template A {
      V1 : text;
      V2 : text;
   }

   match some_predicate
   wrap A ("A", "B");

   match some_other_predicate
   wrap A (V1 => "A", V2 => "B");

values of template variables can be computed with arbitrary expressions, in 
particular using the currently iterated self element or any previously captured
value. For example:

.. code-block:: text

   template w_Name do
      name : text;
   end;

   match DefiningName ("A_Name_(.*)") 
   wrap w_Name ("\1");

When instantiating a template through a wrap or weave clause, or updating a 
template through a weave clause, it is possible to reference fields of that
template as it's being created / modified. In order to acheive that, the 
template under creation need to have its value captured. For example:

.. code-block:: text

   template A {
      V1 : text;
      V2 : text;
      V3 : text;
   }

   match some_other_predicate
   wrap a: A (
      V1 => "A", 
      V2 => "B",
      V3 => a.V1 & "-" & a.V2);

Text Reference Evaluation
-------------------------

There is a fundamental difference to understand between text and string 
fields. String is a direct value. When referenced in an expression, its value
is evaluated directly and cannot change over time. Text is a text reference. 
When referenced in an expression, a pointer to that text is created. This 
pointer will only be resolved upon evaluation of the actual string.

For example:

.. code-block:: text

   template A {
      V1 : text;
      V2 : text;
      V3 : text;
   }

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

# TODO string is not yet implemented

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
created through the new () operator. 

new () can be invoked in match and pick clauses. In a match clauses, it's always
return a reference to the object evaluated (and is therefore evaluated to true).
In a pick clause, the value returned by new will then be the target of the 
controlled actions.

In its simplest form, a new () operator contains a template instantiation 
expression similar to the one of a wrap and weave clause, which value can be 
captured. The only difference is that no origin is set, and therefore no peer
exist. For example:

.. code-block:: text

   template A {
      var V : text;
   }

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

Allocator new also allows to create an tree structure at once, with square 
brackets. Comma separated elements belong to the same level, each of them 
being optionally followed by a square bracked pair to describe its children. 
For example:
 
#TODO: on the first example, which value is being picked? all 3 root ones
presumably? In a match, which value is returned? the last one?

.. code-block:: text

   # creates three siblings "A", "B" and "C"
   pick new ([A ("A"), A ("B"), A ("C")]).

   # creates a root "A" and two children "CHILD A" and "CHILD B", with 
   # "CHILD B" captured on x. 
   pick new (A ("A") [A ("CHILD A"), x: A ("CHILD B")])

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

   template w_PackageDecl {}
   template w_SubpDecl {}

   match PackageDecl ()
   wrap w_PackageDecl ();

   match SubpDecl ()
   wrap w_SubpDecl();

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

   template A {
      var V : text;
   }

   match something and x: A.find (V ("A"))

In the above example, the first instance of A that has a text "A" for V will
be returned. This can also be used with an extension suffix:


.. code-block:: text

   pick A.find ().all ();
