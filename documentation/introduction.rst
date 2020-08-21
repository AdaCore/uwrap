Introduction
============

Purpose
-------

UWrap is a specialized language that works well for problems that can be 
expressed as operations iterating over one or several tree structures, with 
potentially complex pattern matching, and potentially generating one or several 
tree structures. It provides fundamentals for tunability and extensibility. At 
its core, it iterates over various nodes of a tree, allowing the programmer to
control either I/O or intermediary nodes creation.

Main Language Constructions
---------------------------

The UWrap language is manipulated nodes, organized in tree which can be 
analyzed through relationships (child, parent, next, prev...) or through their
specific contents. In addition, node created through the analysis process can
be created as wrappers to the initial tree, adding additional data to it.

A ``command`` is the instruction by which a node is being analyzed. It's 
composted of 4 sections:

- a defer section, if the command is to be analyzed later
- a match section, which describes the conditons for the node under which the
  command can run
- a pick section, which can change the node that is actually being impacted by
  the command
- a wrap or weave section, which define an operation to perform on that node

Commands are evaluated in the inverse order, from last to first, so that later
commands can override earlier ones. This is one of the fundamental mechanism
allowing extensibility and specialization.

A ``template`` defines a set of data that can be created on a node, together
with a sequence of commands to be applied when created. Once created, template
instances belong to a tree. The information they hold can be updated or 
modified.

UWrap also provides the concept of ``function`` which, as in many languages, 
allows to factorize some operations. In uwrap, their are used for the purpose
of factorizing expressions.

General Program Structure
-------------------------

As input, UWrap takes a tree structure. This tree can be coming from the parsing
of a programming source file for example, or something more data oriented such
as a JSON file. The general structure of the iteration will then be as follows:

- Iterate over all the nodes (the input tree as well as the created one)
- For each visited node, do a combination of three possible actions:
 - Create a new node linked to the current node, a wrapper. This can also be  viewed as a way to decorate a node.
 - Create a new node either in the abstract, or connected to one tree
 - Update an already created node.
- Once all node are visited, process I/O

Wrapping Trees
--------------

The most common way to express a solution of a program is to express it in a 
form of a tree of wrapping nodes, or said differently, to decorate nodes
of the input tree. Conceptually, each tree can be associated with a wrapping 
tree, whose nodes are added and updated through the ``wrap`` and ``weave`` 
commands. For example:

.. code-block:: text

   template X do
      var V: text;
   end;

   match x"A"
   wrap X ("some text");

On the above example, for each node that matches the regular text expression 
"A", a node is added to the corresponding wrapping tree, instantiated after the
node template X. We say that an instance of X is wrapping the input node, and
that this input node is the origin of this X wrapper.

The relation between a tree and its wrapping tree can model various things. A 
wrapping tree can be as simple as error messages associated to given nodes, and
as complex as the result of a code generation operation.

For every given node, there can be more than one wrapping node (but only one 
node of every single type). If more node of a given type are necessary, an easy
way is to create new templates through extension. E.g. if in the above case we
need two instances of X, they can be created and named e.g. X1 and X2:

.. code-block:: text

   template X do
      var V: text;
   end;

   template X1 extends X;
   template X2 extends X;

   match x"A"
   wrap X1 ("some text");

   match x"A"
   wrap X2 ("some other text");

Tree relationships (parent, child, sibling...) between nodes in the wrapping
tree are the same as in the original tree. This mean that a wrapper node may
have several parents, if the original tree has more than one wrapper for a given
parent).

Note that wrapping trees may themselves hold wrapping nodes, so there can be 
several levels of derivations.

While there are other ways to describe solutions when this is not sufficient 
(as explained in the next chapter), uwrap programs should always attempted to be
designed around the concept of a wrapping tree. This will provide the easiest
way to structure the result and navigate between input and output. In other
words, the first question should be: can I describe the output tree as a 
projection of the input tree?

Non-Wrapping Nodes
------------------

In some situation, it's more convenient to be able to create manually tree or 
sub-tree structures. This can be done either alongside of a wrapping strucure,
or completely separately. As stated before, this should not be the first choice,
but may turned out to be necessary. For example, in the context of a code 
translation operation, certain nodes of the resulting tree may not correspond
directly to the input tree, but be generated as additional children to the
wrapping trees.

Non-wrapping nodes are created with the ``new`` function, which is able to 
create an entire non-wrapping tree at once. They can either be added to:

- An input tree
- A wrapping tree
- The standalone environment
- A tree that has been created in the standalone environment

Non-wrapping nodes behave exactly like regular wrapping node or input nodes. 
When created as part of an input tree, nothing differenciates them from the 
input structure but for their types. When created as part of a wrapping tree,
the only difference between them and a regular wrapping node is that their
origin is ``hollow``, that is they don't relate to a real wrapped node.
They can however be retreived through the same browsing generators, e.g. 
``child``, which allows a tree with a mix of wrapping and non wrapping nodes to 
be processed regardless of the origin of its nodes.

Gathering the Result of a Program
---------------------------------

Once the computation is over, data of various nodes is aggregated through 
various techniques, and either pushed to a file or on the standard out. 
There are two main ways to aggregate data:

- During wrapper post processing. A child wrapper can update a parent wrapper, concatenating some result. For example:

.. code-block:: text

      template X do
         var Vx: text;
      end;

      template Y do
         var Vy : text;
      end;

      match y: Y()
      pick parent (X)
      weave (Vx => @ & y.Vy);

- Through a reduction function. For example:

.. code-block:: text

      template X do
         var Vx: text;
      end;

      template Y do
         var Vy : text;
      end;

      match X()
      weave (Vx => child (y: Y()).fold ("", @ & y.Vy));
