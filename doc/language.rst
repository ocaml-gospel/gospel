Language Specification
======================

Gospel annotations are inserted in interface files (``.mli``), using
attributes ``[@@gospel "...."]``.  Gospel comes with a preprocessor
for the OCaml compiler that simplifies the writing of such a
specification as ``(*@ .... *)``. We use the latter in the examples
below.

Lexical Conventions
-------------------

Gospel uses the OCaml lexical conventions, with the following
exceptions:

.. TODO

Terms and Formulas
------------------

.. TODO

.. productionlist::
    term: `integer`   ; integer constant
        : | `real`   ; real constant
        : | "true" | "false"   ; Boolean constant
        : | `term` "+" `term`
        : | ..TODO..
   formula: TODO
   pattern: TODO

Function Contracts
------------------

An OCaml function is given a formal specification by appending one
Gospel function contract to its declaration.  Here is an example:

.. code-block:: ocaml

  val eucl_division: int -> int -> int * int
  (*@ q, r = eucl_division x y
      requires y > 0
      ensures  x = q * y + r
      ensures  0 <= r < y *)

Such a contract is composed of two parts:
 - The first line is the header of the contract; it names the function
   arguments and result. It is mandatory and must appear at the
   beginning of the contract.
 - The next lines contain as many specification `clauses` as
   needed. Here we have three clauses: one precondition introduced
   with ``requires``, and two postconditions introduced with
   ``ensures``.

.. productionlist::
    contract: `header` `clause`*
    header: (`ident_tuple` "=")? `identifier` `parameter`+
    clause: "requires" `formula`
        : | "checks" `formula`
        : | "ensures" `formula`
        : | "raises" `case` ("|" `case`)*
        : | "modifies" `term` ("," `term`)*
        : | "equivalent" `string`
        : | "diverges"
        : | "consumes"
    case: `pattern` "->" `formula`
    ident_tuple: `identifier` ("," `identifier`)*
    parameter: "()" | `identifier` | "~" `identifier` | "?" `identifier`

.. TODO ghost parameters and result
.. TODO contract for a constant e.g. val x: int (header needed? etc.)

.. rubric:: Docstrings

Note that Gospel annotations can be combined with traditional
docstrings, e.g. as follows:

.. code-block:: ocaml

  val eucl_division: int -> int -> int * int
  (** this is an implementation of Euclidean division *)
  (*@ q, r = eucl_division x y ... *)

.. rubric:: Absence of contract

.. TODO no contract means anything is possible

.. index:: requires

Precondition (``requires``)
***************************

``checks`` variant

.. index:: ensures

Postcondition (``ensures``)
***************************

here is an example:

.. code-block:: ocaml

  val f: int -> int
  (*@ y = f x
      requires x > 0 *)

.. index:: raises

Exceptional Postcondition (``raises``)
**************************************

when multiple ``raises`` clauses are given, they are checked
independently of each other

each "raises" clause must perform an exhaustive pattern matching on
its argument

.. index:: equivalent

Equivalent
**********

.. TODO

.. index:: diverges

Non termination
***************

.. TODO

.. index:: modifies
.. index:: consumes

Effects
*******

.. TODO

``modifies``...
``consumes``...


.. index:: model
.. index:: mutable
.. index:: invariant
.. index:: ephemeral

Type Specification
------------------

.. TODO

.. index:: function
.. index:: predicate
.. index:: axiom
.. index:: coercion

Ghost and Logical Declarations
------------------------------

.. TODO


