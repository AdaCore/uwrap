Standard Functions
==================

Lambda Expressions
------------------

Lambda expressions can be introduced to store in an expression a sub-expression
that will be only evaluated when resolving to a string. The general syntax is:

.. code-block:: text

   lambda (<some expression>)

Lambda will capture the current ``self`` value, as well as a reference to all
symbols directly available. A common usage of lambda expressions is to compute
a piece of text after wrappers that have not been generated. Consider the 
following example:

.. code-block:: text

   template w_SubpDecl {
      var w_first_parameter : text;
   }

   template w_ParamSpec {
      var name : text;
   }

   match SubpDecl () 
   wrap w: w_SubpDecl ( 
      w_first_parameter => lambda (w.child (w_ParamSpec ()).name)
   );

   match ParamSpec ()
   wrap w_ParamSpec ("some name");

   match w_SubpDecl()
   wrap standard.out (w_first_parameter);

At the point where ``SubpDecl`` is wrapped, ParamSpec hasn't been wrapped yet,
and there's no w_ParamSpec () child to be found. It's later set by ParamSpec,
so that at the time where the value will be converted to a string (during
final processing of standard.out), the lambda will be executed and find 
the expected child.

Fold
----

Fold allows to take a generator function and to reduce it into one single 
value. It is written as a suffix of such generator, for example ``child``.
Folding is computed on capture variables. The general syntax is:

.. code-block:: text

   <prefix generator>.fold (<initial value>, <folding expression>)

For example:

.. code-block:: text

   child (v: true).fold (i: "", i: (i & ", " & v));
   
The above would use i as an accumulator, and take successive values of the
content of child to concatenate into a result. If fold is the last call of
a selector, and the result of the selector is captured, then early evaluation
of that value can be used to write the above expression differently:

.. code-block:: text

   i: child (v: true).fold ("", i & ", " & v);

Functions on Strings
--------------------

The following functions are available for various text formats:

TODO: implementation to be finalized

- text (<some value>): ensures that the resulting value is interpeted as text.
  The function will actually create a dynamic conversion which will only be 
  resolved when converted to an actual string.
- string (<some value>): resvolves the value in parameter to a final string, 
  resolving all the components it's computed from.
- to_lower (<some value>): resolves a string and converts it to lower case
- to_upper (<some value>): resolves a string and converts it to upper case
- normalize_ada_name (<some value>): resolves a string and convert it to following
  the casing of Ada identifiers
- replace_text (<value>, <pattern>, <by>): resolves a string and replace all
  occurences of pattern by a certain value. TODO: should be replace_string and
  replace_string_all instead.
