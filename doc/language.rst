Language Specification
======================

Specification locations
-----------------------

General conventions
^^^^^^^^^^^^^^^^^^^

Gospel annotations are written in interface files (``.mli``).

:ref:`OCaml attributes
<https://caml.inria.fr/pub/docs/manual-ocaml/attributes.html>` with the
identifier `gospel` are used to bear the Gospel specifications in their payload,
as strings: ``[@@gospel "<spec>"]`` or ``[@@@gospel "<spec>"]``.

.. rubric:: Floating attributes

:ref:`Ghost and logical declarations <Ghost and Logical Declarations>` must lie
in floating attributes, inside module signatures::

  [@@@gospel "val f : int -> int"]
  [@@@gospel "predicate is_zero (x: integer) = x = 0"]


.. rubric:: Attached attributes

Specification bits which are semantically attached to OCaml declarations, e.g.
:ref:`function contracts <Function Contracts>` or :ref:`type specifications
<Type Specification>` specifications. In that case, the Gospel specification
should be written in an attached attribute, following OCaml's attachement rules::

  val f: int -> int
  [@@gospel "y = f x ensures x > 0"]

.. rubric:: Specification of ghost and logical Declarations

When ghost and logical declarations need to be specified with a contract, the
contract should reside in an attribute attached to the string containing the
declaration::

  [@@@gospel "val f : int -> int"
    [@@gospel "y = f x ensures x > 0"]]

Gospel preprocessor: ``gospel_pps``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Writing attributes can be tedious, especially when nested. Gospel is provided
with a preprocessor that lets you write Gospel specifications in special
comments, starting with a ``@``::

  val f: int -> int           (* An OCaml value declaration *)
  (*@ y = f x                 (* Its Gospel specification   *)
      ensures x > 0 *)

  (*@ type t *)               (* A ghost type declaration   *)
  (*@ model size: int *)      (* Its Gospel specification   *)

This notation will be used throughout the documentation, in place of the
attribute-based one.


Lexical Conventions
-------------------

Gospel uses the OCaml lexical conventions, with the following
exceptions:

.. todo:: Define lexical conventions

Terms and Formulas
------------------

.. todo:: Define terms and formulas - this is just a placeholder

.. productionlist::
    term: `integer`   ; integer constant
        : | `real`   ; real constant
        : | "true" | "false"   ; Boolean constant
        : | `term` "+" `term`
        : | TODO
   formula: TODO
   pattern: TODO

.. todo:: ``&&`` and ``||`` are lazy, but ``/\`` and ``\/`` are not

Function Contracts
------------------
.. todo:: contracta for a constant e.g. val x: int (header needed? etc.)

An OCaml function is given a formal specification by appending one Gospel
function contract to its declaration. Here is an example::

  val eucl_division: int -> int -> int * int
  (*@ q, r = eucl_division x y
      requires y > 0
      ensures  x = q * y + r
      ensures  0 <= r < y *)

Such a contract is composed of two parts:
 - The first line is the header of the contract; it names the function arguments
   and result. It is mandatory and must appear at the beginning of the contract.
 - The next lines contain as many specification `clauses` as needed. Here we
   have three clauses: one :ref:`precondition <Preconditions>` introduced with
   ``requires``, and two :ref:`postconditions <Postconditions>`" introduced with
   ``ensures``.

.. productionlist::
    contract: `header` `clause`*
    header: (`ident_tuple` "=")? `identifier` `parameter`+
    clause: `precondition`
        : | `postcondition`
        : | `exceptional_postcondition`
        : | "modifies" `term` ("," `term`)*
        : | "equivalent" `string`
        : | "diverges"
        : | "consumes"
    ident_tuple: `identifier` ("," `identifier`)*
    parameter: "()" | `identifier` | "~" `identifier` | "?" `identifier`

.. todo:: ghost parameters and results

.. note::

   In the absence of a contract attached to a function declaration, **no
   assumptions are made** on the behaviour of the function.

   No preconditions or postconditions are to be verified, and the function may
   diverge, raise unlisted exceptions, or modify mutable types, etc.

   One may still enable the implicit specification about exceptions,
   mutability, non-termination, etc. by creating a contract with no clause::

     val eucl_division: int -> int -> int * int
     (*@ q, r = eucl_division x y *)

   Here, it means that ``eucl_division`` terminates, does not raise
   any exception, and does not have any visible side effect.

.. rubric:: Docstrings

Note that Gospel annotations can be combined with traditional docstrings, e.g.
as follows::

  val eucl_division: int -> int -> int * int
  (** this is an implementation of Euclidean division *)
  (*@ q, r = eucl_division x y ... *)


.. index:: requires
.. index:: checks

Preconditions
^^^^^^^^^^^^^

Preconditions are properties that are expected to be verified at function
entry. They are used to describe requirements on the inputs of the
function, but also possibly on a global state.

They are denoted using the keyword ``requires`` or ``checks``, followed by a
:token:`formula`:

.. productionlist::
  precondition: "requires" `formula`
            : | "checks" `formula`

.. rubric:: ``requires``

The ``requires`` clauses state the conditions for which the specified
function has a well specified behaviour.  Whenever a ``requires``
precondition is violated during a call to the function, its behaviour
becomes unspecified, and the call should be considered faulty.  Even
if the call terminates, any other information provided by the contract
(postconditions, exceptions, effects, etc.) cannot be assumed.

In our example, the precondition :math:`y > 0` is required to
ensure the correct behaviour of the function:

.. code-block::
   :emphasize-lines: 3

   val eucl_division: int -> int -> int * int
   (*@ q, r = eucl_division x y
       requires y > 0
       ... *)


.. rubric:: ``checks``

Similarly to the ``requires`` clauses, ``checks`` preconditions should
be met at function entry.  However, unlike ``requires`` clauses, the
behaviour of the function is specified in case such a precondition is
violated. In that case, the function must *fail* by raising an OCaml
``Invalid_argument`` exception, without modifying any existing
state. The call is not considered faulty, but the caller is now in
charge of handling the exception.

The same function contract, where ``requires`` is replaced with ``checks``,
states that the function should raise ``Invalid_argument`` whenever :math:`y
\leq 0`.

.. code-block::
   :emphasize-lines: 3

   val eucl_division: int -> int -> int * int
   (*@ q, r = eucl_division x y
       checks y > 0
       ... *)

.. rubric:: Multiple preconditions

Whenever multiple preconditions of the same kind are provided, they are
verified as a conjunction, which means::

  (*@ ...
       requires P
       requires Q *)

is equivalent to::

  (*@ ...
       requires P /\ Q *)

However, splitting the specification into several, smaller clauses
leads to better readability and maintainability and is encouraged.

.. todo:: what about requires+checks? does the order matter?

.. index:: ensures

Postconditions
^^^^^^^^^^^^^^

Postconditions are properties that are expected to be verified *after* a valid
function call. They are used to specify how the outputs of the function
relate to its inputs, and how values were mutated, when applicable.

Postconditions are denoted using the ``ensures`` keyword, followed by a
:token:`formula`:

.. productionlist::
  postcondition: "ensures" `formula`

As discussed in the :ref:`previous section <Preconditions>`, the
property expressed by the formula is expected to be verified after the
function call only if the preconditions were satisfied.

.. note::

  When an exception is raised, the postconditions are **not** expected to be
  verified. :ref:`Exceptional postconditions` must be used instead.

.. rubric:: Multiple postconditions

The handling of multiple postconditions is identical to preconditions; multiple
postconditions can be merged into a conjunction::

  (*@ ...
       ensures P
       ensures Q *)

is equivalent to::

  (*@ ...
       ensures P /\ Q *)


.. index:: raises

Exceptional postconditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

Exceptional postconditions are used to specify the exceptions that can be raised
by the function. When no such clause is provided, the function should not raise
any exceptions, and doing so is considered a violation of the specification.
Whenever a function can raise an exception as part of its expected behaviour,
this exception must be listed, along with the properties that are verified in
that case.

These clauses are expressed with a ``raises`` keyword, followed by a
list of :token:`cases <case>` associating each exception with its
:token:`formula`, with a syntax similar to OCaml's pattern matching:

.. productionlist::
    exceptional_postcondition: "raises" `exn_case` ("|" `exn_case`)*
    exn_case: `qualid` "->" `formula`
      : | `qualid` `pattern` "->" `formula`
      : | `qualid`

Gospel expects each ``raises`` clause to perform an exhaustive pattern
matching for each exception listed in this clause. Similarly to
OCaml's pattern matching, when an exception is raised, the
postcondition that is satisfied is the first one being matched in the
list of the cases. For instance, the contract::

  (*@ ...
      raises Unix_error (ENAMETOOLONG, _, _) -> P
           | Unix_error _                    -> Q *)

states that only ``P`` holds whenever ``Unix_error`` is raised with
argument ``ENAMETOOLONG``, and that only ``Q`` holds whenever
``Unix_error`` is raised with a different argument.

.. rubric:: Multiple exceptional postconditions

When multiple such clauses are given, they are checked independently
of each other, meaning that the raised exception is matched against
each ``raises``'s case list, and each matching postcondition must be
verified in conjunction. For instance, the contract::

  (*@ ...
     raises Error "foo" -> P | Error _ -> Q
     raises Error x -> R *)

implies that
 - when ``Error "foo"`` is raised, both ``P`` and ``R`` hold, but not ``Q``;
 - when ``Error`` is raised with with an argument different from
   ``"foo"``, both ``Q`` and ``R`` hold, but not ``P``.

.. index:: Out_of_memory
.. index:: Stack_overflow

.. rubric:: Exemptions

Some exceptions are not expected to be listed, because they could be
unexpectedly triggered depending on the specifics of the machine the
code is executed on.  There are two such exceptions in Gospel:
`Stack_overflow` and `Out_of_memory`.

These exceptions are always assumed to be possibly raised by any
function, without an explicit ``raises``. This is equivalent to adding a
``raises Out_of_memory | Stack_overflow -> true`` clause to every function
contract.

Of course, one may still override that behaviour by stating a property
whenever these exceptions are raised, like any other exception.
For instance, one may state that a function runs in constant stack
space as follows::

  (*@ ...
      raises Stack_overflow -> false *)


.. index:: equivalent

Code equivalence
^^^^^^^^^^^^^^^^

.. todo:: do it

.. index:: diverges

Non termination
^^^^^^^^^^^^^^^

.. todo:: do it

.. index:: modifies
.. index:: consumes

Effects
^^^^^^^

.. todo:: do it

.. todo:: when we have ensures+raises+modifies, the effect stated by
          modifies applies for both ensures and raises

``modifies``...
``consumes``...


.. index:: model
.. index:: mutable
.. index:: invariant
.. index:: ephemeral

Type Specification
------------------

.. todo:: do it

.. index:: function
.. index:: predicate
.. index:: axiom
.. index:: coercion

Ghost and Logical Declarations
------------------------------

.. todo:: do it
