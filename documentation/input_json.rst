JSON Input Format
=================

JSON is one of the simplest input format that can be used. It's specified
by the language qualifier "json" on the input. JSON nodes are organized as a 
simple hierarchy, fields and array components being set as direct children of 
the parent node.

JSON provides only three specific predicates:

* name: the name of the node.
* kind: the kind of the node, lower case.
* value: the value of the node. This will be interpreted differently depending
  of the kind:
  * for single values such as String or scalars, it's a predicate on the actual
    value. (TODO: to complete, only string and integer are implemented)
  * for composite value such as arrays and objects, it's a predicate going
    through direct children, equivalent to ``child (\\ true)`` 
    (TOTO: to implement).
