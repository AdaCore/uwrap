********
Tutorial
********

Introduction
============

UWrap is a tool providing two essential capabilities: a tree matching language,
that allows to identify tree nodes and structure, and a wrapping language, 
which allows to associate an input tree with another set of related nodes. This
tutorial will go through the basic of the language, using an Ada appliction
as a mean to generate a tree.

Displaying names
================

The initial setp is to retreive an Ada application that we're going to feed to 
uwrap. The project <link to ada tutorial repo> will do. Once you get this 
project, to to its directory and run:

.. code-block::text

    $ uwrap -l ada -P prj.gpr -w names.wrp src/test.ads

the -l switch specifies the input language. -P refers to a GNAT project listing
various availables sources. The -w switch identifies the wrapping script to 
execute. Finally, a list of source files, here src/spec.ads, provides the
list of files to load, parse and wrap.

Executing this command should list all entities declared within src/spec.ads. 
The file tutorial.wrp looks like this:

.. code-block::text

   match DefiningName ()
   wrap with standard.out (self & "'\n");

The above represent a UWrap command. It's composed in three parts: a matching
expression, the identificaton of a node to wrap, and then a wrapping operation.

.. code-block::text

   [match <matching expression>]
   [wrap [<node to wrap>]
   with <wrapping operation>];

Each section surrounded by [] in the description above can be omitted. Here, 
we're ommiting the "node to wrap" section, which is automatically set to self.

UWrap will traverse the tree node by node, through a depth traversal, examining
the parent before its children. Understanding the order of the traverse is
key to writing more complex programs, where one iteration may depend on either
the previous ones or the next (there's provision in UWrap to refer to objects
not yet created, but let's not get ahead of the flow).

The matching expression ``match DefiningName ()`` matches any node that can be of
the form DefiningName. In the case of Ada, the type of the node as described
by the libadalang library will match for that particular node. A good way
to understand the structure of the Ada matcher is to use GPS and open the 
libadalang view. It will display the tree of Ada nodes as well as their type.

The node currently iterated over is referenced by ``self``. When not specified,
this is the value taken automatically by the ``node to wrap`` expression.

``with`` introduces the name of a template to use to wrap the node. In this
case, ``standard.out`` is a pre-existing template provided by the standard UWrap
libray.

``standard.out`` has a unique field that corresponds to a text to display at the
end of the wrapping process. In this case, the expression ``self & "\n"`` will
convert self to a string - which for Ada nodes means extracting the text of the
node - and then concatenating it with a end of line.

It's useful to undersand that ``standard.out`` is not an operation that is 
instantaneously displaying text. Instead, it's creating a template instance - or
wrapper - around the node, after the template ``standard.out``. Once the 
wrapping operations are over, UWrap will browse through all the created 
wrappers and display the contents of those created after ``standard.out``. This
has various consequences that we'll talk about later on.

Identifying unaliased parameters
================================

Let's look at a more complex matching case. Coding standard implementation is
a good way to illustrate some of the basic capabilities. In this example, we 
have a coding rule that states that parameters should not be marked "access" if
they are only refered to through .all notation.

Open the code under tutorial/access, and run the test:

.. code-block::text

    $ uwrap -l ada -P prj.gpr -w names.wrp src/test.adb

You should see:

.. code-block::text
   V2:V2: access object should be out or in out

Indeed, that V2 parameter is only referenced through ``.all``, or said 
otherwise, it is not referenced other than a ``.all`` reference. It's in
contradiction with the coding standard rule.

Let's open access.wrp and see how this is done:

.. code-block::text

   match param: ParamSpec (" access ") and parent (subp: SubpBody()) {
      match not subp.child (
         Identifier () 
         and not parent (DefiningName())
         and not parent (ExplicitDeref())
         and p_referenced_decl (param))
      wrap with standard.out 
        (":\e<self.child (DefiningName())>: access object should be out or in out\n");
   }

Looks a lot more comprehensive that the previous one, right? Thankfully, it's 
not that complicated, and with a little bit of trial and error, with the help
of the GPS libadalang view, it's relatively easy to develop these kind of 
matcher. Let's look at them step by step.

First:

.. code-block::text

   match param: ParamSpec (" access ") and parent (subp: SubpBody ())

We want to only match param specifications, hence ``ParamSpec``. Furthermore,
the potentially problematic specifications are access mode. There are various
ways to detect these. Here, we're using a textual matching. ``(" access ")``. So
any parameter specification that contains the text access will potentially be
flagged. This is potentially weak (e.g. it doesn't match ``ACCESS`` or 
``:access`` ) but that'll do for the purpose fo the demonstration.

We are going to need to use the value of that node later on the analysis. So
we're capturing it under the ``param`` name. The expression 
``param: ParamSpec (" access ")`` check self against the ParamSpec predicate, 
then stores self in param if matched.

We then need to concentrate only on subprogram bodies - our analysis is going
to cover the usage of these parameters. The second part of the condition is
using a parent predicate, which will look at all parent node, from the direct
parent to the root of the tree. We want to check that there's a ``SubpBody`` 
node in the parent chain. We're going to capture that value in the ``subp`` 
variable, which can be done either on the return of parent, or in the condition
of parent on the return of the predicate SubpBody () (the second option is
retained here).

The curly bracket introduce a list of sub-commands. If the top command is
valid, then the subcommand are executed. Subcommands can be used to describe
more complex logic (there may be more than one command) or just for organization
purposes. Here, it allows to clearly differenciate the parameter that we check
from the analysis of its usage, but is not stricly necessary (we could have
a unique and larger match expression instead).

The matching block looks like

.. code-block::text

   match not subp.child (
      Identifier () 
      and not parent (DefiningName())
      and not parent (ExplicitDeref())
      and p_referenced_decl (param))

Now we need to look at all the node underneath the subprogram declaring this
parameter. We're re-using the node captured before under subp, and through
dot notation, are querying all of it children. We're looking specifically for
a node that:

 * Is an identified: ``Identifier ()``
 * Isn't a declaration, or not a child of a defining name: ``not parent (DefiningName ())``
 * Isn't a dereference, or not a child of explicit deref: ``not parent (ExplicitDeref ())``
 * Is a reference to the parameter param initially captured: ``p_referenced_decl (param)``

A few notes here:

 * ``p_referenced_decl`` is a standard libadalang property query. It does not
 operate on declarations, which is the reason why we have to guard on 
 DefiningNames before.
 * Withing a browsing predicate such as ``child`` or ``parent``, the value of
 ``self`` is switched to the sub-nodes being browsed. So in that second
 child query, p_referenced_decl operates on the child being analyzed, not the
 top level node which is a parameter specification. This is the reason why we
 had to capture the value in the top level matched, then to re-inject it in
 the ``referenced_decl`` call for comparison.

If any node of the form above is found, we're good. There is indeed a reference
to this parameter as an access value, and access mode can be justified. If not,
we will create a message wrapper:

.. code-block::text

    wrap with standard.out 
     ("\e<sloc>:\e<self.child (DefiningName())>: access object should be out or in out\n");

The above demonstrates the usage of the "\e<>" expression in strings."\e<" 
introduces a section of expression, which allows to include in long string
pieces directly computed from the environment without having to concatenate
various pieces. This can be particularly useful when working with multi-lines
strings (openned and closed by """).

Advanced Ada user may have already identified the fact that this implementation
may be a bit naive. It may be useful to consider more situation, for example in
cases where dereference is implicit. The point of this tutorial isn't to show
full Ada awareness, but rather to demonstrate how to write relatively non-trivial
analysis in a few lines of code.

Generating an Ada wrapper
=========================

So far, we have only looked at the matching language, only for the purpose of
displaying messages on the standard output. While this is a perfectly honorable
usage, UWrap is design with wrapping in mind. For that purpose, it comes with 
a standard runtime that facilitates wrapping around the Ada language.

Open the code under tutorial/wrap_names, and run the test:

.. code-block::text

    $ uwrap -l ada -P prj.gpr -w wrap_names.wrp src/test.ads

This should generate Ada files in the local directory. This file contains 
function wrappers - every function calling its counterpart declared in test.ads,
but under different types, parameters and subprogram names.

Let's open wrap_names.wrp and see how this is done:

.. code-block::text

   import ada.wrappers;

   wrap with wrap_ada_specs ();

   match DefiningName ("Some_(.*)"))
   wrap with w_DefiningName ("My_\1");

   match DefiningName ("Some_(?<a>.*)")) and parent (ParamSpec ())
   wrap with w_DefiningName ("A_Param_\e<a>");

First, you'll notice ``import ada.wrappers`` which references a module from
the standard UWrap library. As for languages such as Java, a UWrap scrip always
has access to all its standard library. As a matter of fact, we've been using
it when writing ``standard.out`` before, using the ``out`` template of the module 
``standard``. Using an ``import`` clause allows to refer to the entities declared
in that module without having to prefix.

The next call is:

.. code-block::text

   wrap with wrap_ada_specs ();

This is a conditionless wrapper. This means that every node will be potentially
wrapped by this action. Here, wrap_ada_specs is actually not a template, it is 
a visitor declared in ada.wrappers. Its role is to further explore the current
node and position many default wrappers to it, in order to sustain the generation
of the overall Ada code. This is a good demonstrator of some of the most advanced
capabilities of UWrap - you can open the file [link to include/templates] for
more information. Note that as of today, it is primarily designed to be used
in conjunction to -fdump-ada-spec, and only supports the subset of specification
features that are generated by this option.

This line on its own is already a functionning wrapper code, which will take
a specification and create a wrapper around it, not changing anything. The next
line is instructing to alter the way the default wrapper works:

.. code-block::text

   match DefiningName ("Some_(.*)"))
   wrap with w_DefiningName ("My_\1");

The matcher here introduces regular expressions - we're matching any 
DefiningName that has Some\_ in its name followed by zero or more characters. 
This name is then captured as the first captured element, to be re-used later
on with the "\1" string reference.

We then wrap with w_DefiningName, providing a value "My\_\1", so essentially 
changing Some\_ by My\_, and ignoring any character before Some\_. 
``w_DefiningName`` is a template defined in ``ada.wrappers`` which gets analyzed
at the end of the wrapping process to generate a new name for a given entity.

Writing our own wraping with ``w_DefiningName`` has for effect to override the
default behavior of the standard wrappers. Indeed, there is also a command to 
wrap ``DefiningName`` with ``w_DefiningName`` in ``wrap_ada_specs``. However, 
wrapping operations are evaluated from last to first - with a rule that a given
template can only be wrapping once a given node. So for the entities where our
specific rule matches, no other ``w_DefiningName`` wrapping operation will 
apply, and in particular none of the ones that are declared in ``wrap_ada_specs``.

This effect is more visible by considering the two wrapping operations in this
file:

.. code-block::text

   match DefiningName ("Some_(.*)"))
   wrap with w_DefiningName ("My_\1");

   match DefiningName ("Some_(?<a>.*)")) and parent (ParamSpec ())
   wrap with w_DefiningName ("A_Param_\e<a>");

In this sequence, we will first evaluate wether we are on a defining name
child of a parameter which matches Some\_. If that's the case, we'll wrap the
name to "A_Param\e<a>" and the wrapper above will not be executed. If we're
not on a parameter of the correct name, then we'll check if the matcher above
can be executed. And if not, the top one in ``wrap_ada_specs`` will be.

Also note the alternative syntax to capture a name in a regexp on the second
command. Often with wrapping programs, many regexps needs to work in conjunction
with the other with many pieces to match. It can be difficult to track the 
group numbers, so the form "(?<some name>some pattern>)" allows to name a given
group, for re-use in expressions later on.

Wrapping C strings into Ada Strings
===================================

Renaming Ada entities is a fun exercise, but let's look at a real life example.
The initial motivation behind UWrap was to provide a platform to automatically
massage the output of the C to Ada binder fdump-ada-spec (although argulably
there are much more uses cases of it now). Bindings generated by fdump-ada-spec
are extermly useful in the sense that they provide a binary accurate translation
from C to Ada. However, no decision on the semantic of the binding can be 
provided, and C being very low level, it results into a very low level binding
which feels like C even with Ada.

UWrap use case here is to provide a relatively easy way to describe the 
decisions to take as to developer a thicker binding. One of the most common of
these decisions to make is wether a C string should remain a pointer to char,
or if it should be converted to an Ada String - which involved a potentially
expensive operation (a copy) but improves greatly the quality of usage.

Let's have a look. Open the code under tutorial/c_strings and run the following:

.. code-block::text

    $ uwrap -l ada -P prj.gpr -w c_strings.wrp src/test_h.ads

``test_h.ads`` is a pregenerated output of fdump-ada-specs. You'll notice that
this project also has the original C code. The resulting wrapping code is
an Ada package that is calling the originally bound C code, and replacing in a 
few places C strings with Ada strings. Let's look at the wrapper code: 

.. code-block::text

   import ada.wrappers;
   import ada.transformations;

   wrap with wrap_ada_specs ();

   match DefiningName ("(?<n>.*)_h")
   wrap with w_DefiningName (normalize_ada_name(n));

   match ParamSpec() 
      and child (SubtypeIndication("Interfaces.C.Strings.chars_ptr")) 
      and not child (DefiningName ("^leaveMeAlone$"))
   wrap with chars_into_string ();

   match SubpDecl() 
      and child (f_subp_kind ("function"))
      and child (SubtypeIndication("Interfaces.C.Strings.chars_ptr")) 
   wrap with chars_into_string ();

As before, we're going to use ``ada.wrappers`` to invoke ``wrap_ada_specs``. This
time however, we're also going to use ``ada.transformations``. This module
provides a number of pre-set visitors, that are able to do complex modifications
on the generated bound code. Note that it's perfecly fine to describe the fine
behavior of these transformation yourself. However, this requires a deep 
understanding of the way Ada wrapping is setup, while the already provided 
transformation are off the shelf. They can also serve as a base to develop 
custom ones. Description on the way these work go beyond the scope of the
tutorial, and will be covered by the full UWrap documentation.

The first command reads:

.. code-block::text

  match ParamSpec() 
      and child (SubtypeIndication("Interfaces.C.Strings.chars_ptr")) 
      and not child (DefiningName ("^leaveMeAlone$"))
   wrap with chars_into_string ();

This matches a parameter specification, then looks at a child of type
``SubtupeIndication``, which would be the type of the parameter. Here,
we're performing a textual check to the full name of the C char type, which
corresponds to the pattern generated by fdump-ada-specs. We're also then 
describing a condition where we don't want to apply this transformation, if the
defining name of the parameter is exactly "leaveMeAlone". If all these conditions
match, then ``wrap with chars_into_string ()`` will apply the preset 
transformation from C string to Ada string.

To modify a returned type, a transformation needs to be applied directly on the
subprogram itself. This is the role of the code

.. code-block::text

   match SubpDecl() 
      and child (f_subp_kind ("function")) 
      and child (SubtypeIndication("Interfaces.C.Strings.chars_ptr")) 
   wrap with chars_into_string ();

We will here match for a subprogram declaration which is of a function kind
and has a subtype indication (its return type) matching the name of a C string.
The visitor ``chars_into_string`` is versatile enough and knows how to handle
both parameters and visitors.

Some careful reader may have noticed the usage of the predicate ``f_subp_kind``.
This is similar to the property check ``p_referenced_decl`` we used before,
and here means "match a node that has such a field and which field matches 
a specific values". Properties and fields are features of langkit and libadalang
which input tree of UWrap currently relies on.

Going further
=============

While UWrap documentation is still work in progress, and some of its semanics
are still being refined. The language offers much more capabilities such as 
template definition, containers, templates types, control over the iteration,
creation of arbitrary subnodes, matching over the created templates, lambda,
reductions, etc. A good way to have a glance of it is to check out the core
testsuite of the language.

On top of these, a number of Ada transformations are already implemented, 
allowing to transform return integers into exception, access parameters into
returned values or out modes or arrays, etc. A good way to get an idea on how
these work is to look at the fdump-ada-spec specific testuite, or directly
at the implementation of the transformations and ada wrappers.

At the time of writing, a lot for work is still necessary to stabilize the 
language, its processing and error recovergy. Performances have not been 
optimized yet and a few shortcuts may end up to long processing times on 
particulary large input files or complex wrappers. Feel free to open issues 
on the github tracker to report any problem or suggestion!
