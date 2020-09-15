Deferred Computation
====================

Understanding Input Tree Iteration
----------------------------------

One of the most important aspect of the language is to understand the sequence
of processing, and how to create structures which may rely on data that has not
yet been processed.

The input is iterated over through deep tree traversal. That means that in a 
tree like:

.. code-block:: text

   A {
      B,
      C {D, E}
      F
   },
   G

The nodes will be analyzed in the order A, B, C, D, E, F, G. There is some 
control over the seqence of the algorithm through generators. Note that all
the examples in the following section will be based on the above structure.

On a given node, the main program or a specific analysis can be triggered ahead 
of time or in a different order. Such specific traversal is controlled by a 
generator, either a predefined one (such as ``child``) or a custom function,
and the expansion suffix ``.all ()``. For example:

.. code-block:: text

   match "A"
   pick child (x"B").all () wrap standard.root ();

TODO: write tests to check that standard.root behaves strictly like template 
(it probably doesn't). Code that keeps track of iteration id is then probably
unecessary.

will apply the main program on all children of the node named "A" that match
the string pattern "B". 

Using a ``do .. then`` sequence, it's possible to control the order in which
subtrees are analyzed:

.. code-block:: text

   match "A" do
      pick child (x"B").all () wrap standard.root ();
   then
      pick child (x"C").all () wrap standard.root ();
   end;

The above will first process all nodes matching "B" then "C". Node that other
child will also be processed by the above after the command. This can be 
cancelled through iteration control ``wrap over`` which can be placed anywhere
in the command and indicates that when this command executes, no more depth
analysis should be performed:

.. code-block:: text

   match "A" do
      pick child (x"B").all () wrap standard.root ();
   then
      pick child (x"C").all () wrap standard.root ();
      wrap over;
   end;

Deferred Computation
--------------------

While this provides certain control over the iteration, it's usually not 
sufficient to always bring all the information to build the desired output
when processing a specific node. Two methods are available to structure 
deferred analysis:

 - wrapper analysis passes
 - deferred computation

 All rely on the fact that I/Os in UWrap are computed at the very end of the
 entire processing. So for example when you write:

 .. code-block:: text

   match "A"
   wrap standard.out ("Hello World");

No ouptut is performed on the command. Instead a ``standard.out`` wrapper is
created around the node matching the text "A". This wrapper will be captured
when all other processing is fininshed, which will allow information that was
not yet available at the time of the creation of the wrapper to be computed
and used for the output.

Wrapper Post processing
-----------------------

A first way to handle deferred computation is at the template instance level.
Once created, wrappers are scheduled to be processed one all already scheduled 
processing in finished. For example, one can write:

.. code-block:: text

   template X do
      var some_text : text;
   end;

   template Y do
      var some_text : text;
   end;

   match "A"
   wrap X ();

   match "B"
   wrap Y ("Hello ");

   match "C"
   wrap Y ("World ");

   match X ()
   weave (some_text => child (v: Y ()).all().fold (c: "", c: (c & v.some_text & " "));

At the time of the creation of the ``X`` wrapper, no children exist yet. If we
want to, for example, concatenate the value of all children of type Y, we can
match for the wrapper type X, which will be performed while all other nodes
are visited, and in particular when the nodes "B" are processed and trigger the
creation of the Y wrappers.

This technique is particuarly useful when some post processing needs to be
applied on templates, once the rest of the structure computation is over.

Deferred Commands
-----------------

Instead of relying on the automatic iteration over instantiated templates, an
alternative is to explicitely defer the computation of a command. This can
be useful in particular if a template needs to schedule some commands for a 
later point in time where its structure is completed. Deferred commands can
be introduced by the ``defer`` reserved word prior to any other section:

.. code-block:: text

   defer <condition> <command>

If used without a condition, then a deferred command will be executed at the
end of the current iteration. For example:

.. code-block:: text

   match Entity () do
      defer
      match child (wrapper (w_Entity ()))
      wrap X (); 
   end;

In the above example, at the time the outer command is executed, there may not
be a child yet that has a wrapper w_Entity (). The ``defer`` section above will
execute the nested command after all the current nodes are iterated over.

A condition can also be added to the defer section - in which case the defer
command will either be executed if that conditon is true, otherwise it will be
defered again to the end of the current iteration. E.g.:

.. code-block:: text

   match Entity () do
      defer child (wrapper (w_Entity ()))
      wrap X (); 
   end;

Expression in defer section behave the same as those in the match section. In 
particular, they can retreive variables. It is also possible to have in the
same command a conditionned defer section followed by a match section.

When scheduled, defer command capture their environment, which includes in 
particular the value of ``it`` as well as any variable. So one can write:

.. code-block:: text

   match e: Entity () do
      defer e.child (wrapper (w_Entity ()))
      wrap X (); 
   end;

Deferred Expressions
--------------------

An alternative to defer commands is defer expressions. A defer expression is an
expression computed only at the point of conversion to string, and can be used 
in particular when the underlying structure is not yet available, or when the 
values actually need to be known.

This is a common pattern when describing text templates. For example:

 .. code-block:: text

   template X do
      var V1 : text;
      var V2 : text;
      var V3 : text;
   end;

   match "A"
   wrap X (V3 => defer (V1 & V2));

Even if V1 and V2 have no value at the point of the analysis of the A node, V3
is set as a deferred expression concatenating of these two references. In a 
futher calls, we could have:

.. code-block:: text

   weave X (V1 => "Hello ", V2 => "World");

When computing V3 value later, for example in the context of an I/O, the correct
value "Hello World" would be analyzed. 

Defer can contain arbitrary complex expression, and as for defer commands can
be used in particular to describe computation on structure that is not yet
available. For example:

.. code-block:: text

   match "A"
   wrap X (some_text => defer (child (Y ()).all().fold ("", @ & it.some_text & " ")));

Incidentally, when computing large section of text that potentially rely a lot
on deferred data, it's common to prefix the string by defer and let the actual
content depends (or not) of such data, e.g.:

.. code-block:: text

   template C_Function do
      var result : text => "void";
      var name : text => "default_name";
      var parameters : text => "";
      var variables : text => "";
      var statements : text => "";

      var code => defer (i"""
         \e<result> \e<name> (\e<parameters>) {
            \e<variables>
            \e<statements>
         }
      """);
   end;
