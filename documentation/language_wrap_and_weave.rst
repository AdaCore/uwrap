Wrap and Weave Clauses
======================

Overall Structure
-----------------

A wrap or a weave clause allows to perform an operation to a node. This 
operation can be:

- Instancate a template on the current node
- Update a template instance of the current node
- Control the remainder of the iteration from that node

It's written:

.. code-block:: text

   wrap <an optional capture> <a static reference to a template> <parameters>;
   weave <an optional capture> <a static reference to a template> <parameters>;

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

It can stand on its own, in which case it unconditionally operates on ``it``:

.. code-block:: text

   wrap <some expression>
   weave <some expression>;

A wrap or a weave clause is always terminating a command, and is always followed
by a semicolon.

Wrapping and Weaving Templates
------------------------------

Templates are static structural references. A given node can only be wrapped 
once by such entity. When trying to apply wrap a second time to the same node 
will the same entity, the command will skip.

Recall that commands are applied from bottom to top. In the following sequence:

.. code-block:: text

   match x"A"
   wrap standard.out ("TOP");

   match x"B"
   wrap standard.out ("BOTTOM");

The two commands only contain a wrap clause, wrapping with the 
template standard.out. The second one with the text bottom will be executed 
first. When executing the one above, an association between the template 
``standard.out`` and the current node already exists. As a consequence, the wrap
command will be skipped. This behavior is the core that allows refinement of
wrapping sequences. For example:

.. code-block:: text

   wrap standard.out ("THE NAME DOESN'T CONTAIN SPECIFIC PATTERN");
      
   match x"A"
   wrap standard.out ("THE NAME CONTAINS A");

   match x"AB"
   wrap standard.out ("THE NAME CONTAINS AB");

Wrapping clauses can be developped to further refine the expected behavior.

Weave clauses allow to apply more than once a given template. They will either
instantiate a template if not associated with the node yet or update its field.
For example in:

.. code-block:: text

   match x"A"
   weave standard.out ("TOP");
   
   match x"A"
   weave standard.out ("BOTTOM");

The two commands only contain an unconditional wrap clause, weaving with the 
template standard.out. The second one with the text bottom will be executed 
first. The top one will be executed second and update the template instanciated
previously. "TOP" will be displayed on the screen.

Very often, weaving will actually need to update a value rather than overriding 
it. The `@` references the previous value of a given field, so that if in the
previous example we want to display both TOP and BOTTOM, we could have:

.. code-block:: text

   match x"A"
   weave standard.out (@ & " TOP");

   match x"A"
   weave standard.out ("BOTTOM");

Weaving does not prevent wrapping even if it's applied first. The following code
will work the same as the code above even if weave is executed first:

.. code-block:: text

   match x"A"
   wrap standard.out (@ & " TOP");

   match x"A"
   weave standard.out ("BOTTOM");

At the time of its creation, a template instance will execute its sequence of
commands, if any.

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
either a match clause or the scope of a template will result in infinite loops:

.. code-block:: text

   wrap standard.out ("INFINITE LOOP");

the above code will first wrap input nodes with standard.out, then wrap the
resulting standard.out node with another instance of standard.out, and so and 
so forth.

TODO: There is a simple way to warn about the above, to implement.

Controlling Iteration
---------------------

TODO: wrap stop needs to be implemented.

A command is always run within the iteration of either a list or a tree 
structure. In some situation, it's useful to be able to either interup this 
iteration, or in the case of a tree, to skip over the elements. This can be 
controlled with the ``wrap into;``, ``wrap over;`` and ``wrap stop;`` operation.

- ``wrap into;`` is the default state. It means that the current iteration will 
  look a leaves of the current node. 
- ``wrap over`` will prevents leaves to be analyzed. It is the same as 
  ``wrap into`` when iterating over a list.
- ``wrap stop`` interups the current iteration.

Only one wrap iteration decision can be taken for a given node in a given 
interation. For example:

.. code-block:: text

   wrap into;
   wrap over;

Will always apply wrap over. 

Wrap decisions are usually taken in conjunction with other commands, for 
example:

.. code-block:: text

   match some_conditions do
      wrap something;
      wrap over; # OK, no need to look below.
   end;

By default, the main iteration is controlled. However, in the case of a nested
iteration introduced by an ``all ()`` extension suffix, the wrapping control
will apply to that iteration instead, e.g.:

.. code-block:: text

   pick child ().all () do
      match some_condition
      wrap something;
      wrap over; # OK, no need to look below.
   end;
