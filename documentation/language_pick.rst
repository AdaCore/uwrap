Pick Clause
============

Overall Structure
-----------------

A pick clause is a clause that selects objects that the commands action will 
operate on. It's written:

.. code-block:: text

   pick <some expression>

It can be preceded by an optional match clause which will conditionalize and 
parametrize its execution:

.. code-block:: text

   match <some expression>
   pick <some epression>

It can stand on its own, followed directly by a semicolon:

.. code-block:: text

   pick <some epression>;

In this case, no action is applied, but the pick clause can still have side 
effects that are executed under the above syntax.

By default, a command operates on the element currently being iterated which is 
referend to as ``self``, so that:

.. code-block:: text

   pick self

is equivalent to not having a pick clause.

Actions targeted by a pick clause can be a a sequence, a wrap clause or a weave 
clause.

Picking Expressions
-------------------

A pick expression can be build as a match expression, and will switch the 
``self`` value to the rest of the command. For example:

.. code-block:: text

   pick child (BasicDecl ())

will pick the first child of the current node which is a basic declaration. At
the difference of a match expression, it is an error to pick one element that
doesn't exist. This error will result in a message in the output. The fact
that pick expression return an error may favor the use of pick or match
expression to select specific objects depending on the intended behavior.

Picking Matched Object
----------------------

Pick expressions can refer to any value or object matched by the previous 
matcher. For example:

.. code-block:: text

   match x: (child ("A") or child ("B"))
   pick x

Will pick either a child of name "A" or a child of name "B".

Picking Multiple Objects with Boolean Expression
------------------------------------------------

TODO: This is currently not fully working - to fix and test.

A pick expression can select more than one element to apply the following 
actions. This can be done for example through boolean expressions:

.. code-block:: text

   pick a and b

Will apply the following actions to a and b. In the case of an or operator:

.. code-block:: text

   pick a or b

Actions will be operating on a if a is true, otherwise they will be operating on 
b.

Expression can be combined. In the following code:

.. code-block:: text

   pick (a and b) or (c and d)

Actions controlled by pick will either be run on a and b, or on c and d.

Note that only the expression at the top of the pick expression will be 
concerned by this. In particular, nested matching expression will not, so that:

.. code-block:: text

   pick DefiningName (a or b)

the above expression only resolve to one element, which is the current element
``self``.

It is an error to have a pick expression that resolves to false. So if in:

.. code-block:: text

   pick (a and b) or (c and d)

The overall result of this is false, then an error will be displayed on the 
output.

The Expansion Suffix ".all()"
-----------------------------

The ".all()" suffix allows to consider all values that match a given browsing
function prefix. Recall that:

.. code-block:: text

   pick child ()

Only selects the first child of the current node. However:

.. code-block:: text

   pick child ().all ()

Will apply the actions controlled by pick to all children of the current node.
Both child and all can contain a nested predicate. The following two notations
are equivalent:

.. code-block:: text

   pick child (<some predicate>).all ()
   pick child ().all (<some predicate>)

Note that all () doesn't need to be the last element in the selected expression.
The following will pick the first child of all children which contain the letter 
"A":

.. code-block:: text

   pick child( "A").all ().child ()

#TODO: as before, the following doesn't currently completely works:

the expansion suffix all() also works with boolean operators, for example:

.. code-block:: text

   pick child ("A").all () and child ("B").all ()

will pick all children of "A" and "B" (and will raise an error if there's not
chlid of a "B" node or no child of an "A" node).
