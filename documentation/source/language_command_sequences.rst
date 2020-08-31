Nested Commands and Sequences
=============================

Overall Structure
-----------------

A command sequence can be either controlled by a match or a pick clause, 
or be introduced directly in a list of commands. It's also the body of a 
function or a template (described in a later chapters). Command sequences are
required when using alternative eslmatch or else in a match block.

Command sequences are introduced with "do", terminated by "end;" 
and contain a list of commands, possibly separated by "then". For example:

.. code-block:: text

   do
      match a 
      pick b
      wrap c ();
   end;

   match d do
      pick e do

      end;
   end;

   do
      match a 
      pick b
      wrap c ();

      match d 
      pick e
      wrap f ();
   then   
      match d
      wrap e ();
   then
      match f
      pick g;
   end;

At the begining of a sequence, variables can be declared, for example:

.. code-block:: text

  do
      var v1: text;
      var v2: text => "some default value";

      pick b
      wrap c ();

      match d 
      pick e
      wrap f ();
   then   
      var v3: text;
      var v4: text => "some other value";

      match d
      wrap e ();
   end;

These variables can be used after the declaration in further commands, or 
commands followed by a ``then``. If the block is written in a template, these
variables will be stored by the template, modifiable at a later stage.

Evaluation order
----------------

Without ``then`` separations, commands within a sequence are evaluated from 
bottom to top, as in the main program. 

When a ``then`` separates two groups of commands, the first group will be
evaluated first, then the second. This allows to cater for situations where
it's more convenient to have a top to bottom evaluation, in particular when
some commands depend on the other. So in the following example:

.. code-block:: text

   do
      wrap a ();
      wrap b ();
   then
      wrap c ();
      wrap d ();
   then
      wrap e ();
      wrap f ();
   end;

the evaluation order will be: b, a, d, c, f, e.

When nested, commands clauses are evaluated from the outer sections to the 
inner section. In something like:

.. code-block:: text

   match a do
      match b;
   end;

``match a`` is evaluated before ``match b``.

Semantic Scope
--------------

A command and its sequences share the same data frame. In particular, name
captured anywhere in the command or a sequence is shared down and up. So that 
in:

.. code-block:: text

   match x: some_expression do
      match y: some_expression;
      match z: some_expression;
   then
      # here, x, y and z are available if they have been captured above
   end;

This is also true for named textual group capture, such as "(<x>.*)". However,
numbered group capture are only available in the nested commands, not sibling
sections. So that:

.. code-block:: text

   match x"(.*)" do
      match x"(.*)": some_expression 
      # here, "\1" is the outer match capture, "\2" the inner one.
      ;

      # only "\1" is available below, from the outer capture
      match z: some_expression;
   then
      # only "\1" is available below, from the outer capture
   end;
