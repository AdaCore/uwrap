Wrap and Weave Clauses
======================

Overall Structure
-----------------

A wrap or a weave clause allows to perform an operation to a node. This 
operation can be:

- Associate a template instance with the current node
- Update a template instance of the current node
- Call a visitor
- Control the remainder of the iteration from that node

It's written:

.. code-block:: text

   wrap <an optional capture> <a static reference to a template or visitor> <parameters>;
   weave <an optional capture> <a static reference to a template or visitor> <parameters>;

or when controlling the iteration:

.. code-block:: text

   # TODO: wrap stop is not implemented
   wrap <over or into or stop>;

It can be preceded by an optional match clause which will conditionalize and 
parametrize its execution, or an optional pick clause which will select what
node it operates on

.. code-block:: text

   match <some expression>
   pick <some epression>
   wrap <some expression>;

   match <some expression>
   pick <some epression>
   weave <some expression>;

It can stand on its own, in which case it unconditionally operates on self:

.. code-block:: text

   wrap <some expression>;
   weave <some expression>;

A wrap or a weave clause is always terminating a command, and is always followed
by a semicolon.

Wrapping and Weaving Templates and Visitors
-------------------------------------------

# TODO: visitor unicity needs to be implemented.

Visitor and templates are static structural references. A given node can only 
be wrapped once by such entity. When trying to apply wrap a second time to the
same node will the same entity, the command will skip.

Recall that commands are applied from bottom to top. In the following sequence:

.. code-block:: text

   match "A"
   wrap standard.out ("TOP");

   match "B"
   wrap standard.out ("BOTTOM");

The two commands only contain a wrap clause, wrapping with the 
template standard.out. The second one with the text bottom will be executed 
first. When executing the one above, an association between the template 
``standard.out`` and the current node already exists. As a consequence, the wrap
command will be skipped. This behavior is the core that allows refinement of
wrapping sequences. For example:

.. code-block:: text

   wrap standard.out ("THE NAME DOESN'T CONTAIN SPECIFIC PATTERN");
      
   match "A"
   wrap standard.out ("THE NAME CONTAINS A");

   match "AB"
   wrap standard.out ("THE NAME CONTAINS AB");

Wrapping clauses can be developped to further refine the expected behavior.

Weave clauses allow to apply more than once a given template. They will either
instantiate a template if not associated with the node yet or update its field.
For example in:

.. code-block:: text

   match "A"
   weave standard.out ("TOP");
   
   match "A"
   weave standard.out ("BOTTOM");

The two commands only contain an unconditional wrap clause, weaving with the 
template standard.out. The second one with the text bottom will be executed 
first. The top one will be executed second and update the template instanciated
previously. "TOP" will be displayed on the screen.

Very often, weaving will actually need to update a value rather than overriding 
it. The `@` references the previous value of a given field, so that if in the
previous example we want to display both TOP and BOTTOM, we could have:

.. code-block:: text

   match "A"
   weave standard.out (@ & " TOP");

   match "A"
   weave standard.out ("BOTTOM");

Weaving does not prevent wrapping even if it's applied first. The following code
will work the same as the code above even if weave is executed first:

.. code-block:: text

   match "A"
   wrap standard.out (@ & " TOP");

   match "A"
   weave standard.out ("BOTTOM");

Visitor wrapping behave the same way as template instancitations. A given 
visitor can only be applied once on a wrap clause, but multiple times on weave
clauses.

Wrapping and weaving Template Instances
---------------------------------------

When wrapping or weaving a template, an actual node is created and associated
with the iterated node. This template instance node will itself be subject to
its own iteration which will go through the entire program.

That second stage of iteration can be used to create another set of template 
instances, or to further refine the values of the created template, 
using information that may not have been available at first.

Updating a template can be done through the self weave syntax, which is similar
to the weave syntax exept that it doesn't mention a template name. The 
following code for example will provide same result as before:

.. code-block:: text

   match standard.out ()
   weave (@ & " TOP");

   match "A"
   wrap standard.out ("BOTTOM");

Note of the fact that template instances created by weave and wrap clauses will
themselves be subject to the whole program, wrap clauses not contextualized by
either a match clause or the scope of a visitor will result in infinite loops:

.. code-block:: text

   wrap standard.out ("INFINITE LOOP");

the above code will first wrap input nodes with standard.out, then wrap the
resulting standard.out node with another instance of standard.out, and so and 
so forth.

TODO: There is a simple way to warn about the above, to implement.
