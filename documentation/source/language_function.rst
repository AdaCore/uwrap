Functions
=========

Overall Structure
-----------------

As for other languages, functions allow to factorize code. In UWrap, they are 
meant to be used for expressions. The overall syntax is:

.. code-block:: text

   function <name> (<parameters>) do
      <commands> 
   end;

The commands in a function need to be ``pick`` commands (as opposed to ``wrap`` 
or ``weave`` which are no meaning in this context). When called in a context of
a regular call, the first picked value will be returned. When called in the
context of a generator, values will be generated following the same order as
the overall of order of command evaluation.

For example:

.. code-block:: text

   function F () do
      pick "Hello ";
   then
      pick "World"; 
   end;

Will either return "Hello " in a regular call, or "Hello " then "World".

Functions can then be called in expressions, e.g.

.. code-block:: text

   function add_space (p) do
      pick p & " "; 
   end;

   wrap T (v => add_space (@));

Generators
----------

All functions are generators - some only yield one value, some may yield 
several, but they all can be used as prefix or various suffixes supporting
generators, such as ``all``, ``fold`` or ``filter``.

For example, the following function:

.. code-block:: text

   function F () do
      pick "Hello ";
   then
      pick "World"; 
   end;

Yields "Hello " and "World. Functions call themselves call generator. In order
or all values of a called generator to be yeiled, the suffix ``all`` need to 
be used both in the function and its caller. For example:

.. code-block:: text

   function F1 () do
      pick child ();
   end;

Will only pick the first child of the current iterator. But:

.. code-block:: text

   function F2 () do
      pick child ().all();
   end;

Will be able to generate all values. If called outide of the need of a 
gernerator though, only the first value will be picked, so the two calls below
are equivalent:

.. code-block:: text

   pick F1 () wrap X ();
   pick F2 () wrap X ();

However, with e.g. a ``all`` suffix, F1 will only apply the X wrapper on the
first child (it only yeilds one) while F2 will do on all:

.. code-block:: text

   pick F1 ().all () wrap X ();
   pick F2 ().all () wrap X ();
