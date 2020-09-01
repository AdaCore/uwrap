Langkit Input Format
====================

Langkit is a generic framework allowing to define language grammars. It is used
in particular for Ada, as well as for the template language of UWrap itself. 
Uwrap has native support for langkit (even if at this stage new lankit 
generated libraries need to be manually included).

Langkit-generated trees provided the following predicates:

* Predicates on their type and parent types. The name of the predicate is the
  name of the type checked against.
* Predicates on fields. These predicates are prefixed by f_ and the name of the
  field. Their return another langkit node.
* Predicates on properties. These predicates are prefixed by p_ and the name
  of the property. They can have parameters and return nodes or other kind of
  data.

Each node that langkit generates creates a node in the UWrap tree. Contrary
to simpler trees such as JSON, langkit can generate a number of intermediary
nodes that don't direcly appear on the code, which can be confusing. In
particular, grammars tend to have a number of lists. For example, the test 
language organizes nodes in trees, e.g.:

.. code-block:: text

   A {B, C, D}

However, in the above example, B, C and D are not direct children of A (they 
would in an equivalent JSON example). A has a child which is a list, which
then contains A, B and C. Using the child query on the A entity, getting it
direct children needs to be written:

.. code-block:: text

   child (\ true \ true)

and not:

.. code-block:: text

   child (\ true)

which would only select the list.

The following languages are currently included in UWrap:
* Ada, see libadalang for more details
* Template, the language for UWrap itself (TODO)
* Test, a simple language used in the UWrap testsuite
