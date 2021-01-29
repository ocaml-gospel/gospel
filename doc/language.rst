Language Specification
======================

Gospel annotations are inserted in interface files (``.mli``), using attributes
``[@@gospel "...."]``. Gospel comes with a preprocessor for the OCaml compiler
that simplifies the writing of such a specification as ``(*@ .... *)``. We use
the latter in the examples below.

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
    clause: `precondtion`
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

Preconditions are properties that are expected to be verified *before* the
function call. They are usually used to describe the expected inputs of the
function, but may also relate to a global state.

They denoted using the keyword ``requires`` or ``checks``, followed by a
:token:`formula`:

.. productionlist::
  precondition: "requires" `formula`
            : | "checks" `formula`

.. rubric:: ``requires``

The ``requires`` clauses state the conditions for which the specified function
has a well specified behaviour. They unlock the other informations provided by
the specification (postconditions, exceptions, effects, etc.).

In our previous example, the precondition :math:`y > 0` is required to ensure the
correct behaviour of the function:

.. code-block:: ocaml
   :emphasize-lines: 3

   val eucl_division: int -> int -> int * int
   (*@ q, r = eucl_division x y
       requires y > 0
       ... *)

Whenever a ``requires`` precondition is violated during a call to the function,
the behaviour of the function becomes unspecified, and the call should be
considered faulty.

.. rubric:: ``checks``

Similarly to the ``requires`` clauses, ``checks`` preconditions should be
verified in the state of the environment prior to the function call.

However, unlike ``requires`` clauses, the behaviour of the function is specified
in case such a precondition is violated, and the function must *fail* by raising
an OCaml ``Invalid_argument`` exception immediately, and before modifying any
existing state; the prestate is left strictly unchanged. In that case, the call
is not considered faulty, but the caller is now in charge of handling the
exception.

The same function contract, where ``requires`` is replaced with ``checks``,
states that the function should raise ``Invalid_argument`` whenever :math:`y
\leq 0`.

.. code-block:: ocaml
   :emphasize-lines: 3

   val eucl_division: int -> int -> int * int
   (*@ q, r = eucl_division x y
       checks y > 0
       ... *)

.. rubric:: Multiple preconditions

Whenever multiple preconditions of the same kind are provided, they must be
independently verified, in a conjunction, which means::

  val ...: ...
  (*@ ...
       requires P
       requires Q *)

is equivalent to::

  val ...: ...
  (*@ ...
       requires P /\ Q *)

However, splitting the specification of the prestate into smaller properties
leads to better readability and maintainability and is generally encouraged.


.. index:: ensures

Postconditions
^^^^^^^^^^^^^^

Postconditions are properties that are expected to be verified *after* a valid
function call. They are mostly used to specify how the outputs of the function
relate to its inputs, and how values where mutated, when applicable.

They are denoted using the ``ensures``, followed by a :token:`formula`:

.. productionlist::
  postcondition: "ensures" `formula`

Of course, as discussed in the :ref:`previous section <Preconditions>`, the
property expressed by the formula is expected to be verified after the function
call only if the preconditions have been verified.

.. note::

  When an exception is raised, the postconditions are **not** expected to be
  verified, and the prestate must be left unchanged by the call, unless
  specified otherwise in an :ref:`exceptional postcondition <Exceptional
  postconditions>`.

.. rubric:: Multiple postconditions

The handling of multiple postconditions is identical to preconditions; multiple
postconditions can be merged with a conjunction::

  val ...: ...
  (*@ ...
       ensures P
       ensures Q *)

is equivalent to::

  val ...: ...
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

Thses clauses are expressed with a ``raises`` keyword, followed by a list of
:token:`cases <case>` associating each exception with its :token:`formula`
exceptions to be matched, in a syntax similar to OCaml:

.. productionlist::
    exceptional_postcondition: "raises" `case` ("|" `case`)*
    case: `pattern` "->" `formula`

Gospel expects ``raises`` clauses to perform an exhaustive patterm matching each
exception's arguments. Similarly to OCaml's pattern matchings, when an exception
is raised, the postcondition to be verified is the first one being matched in
the list of the cases.

.. rubric:: Multiple exceptional postconditions

When multiple such clauses are given, they are checked independently of each
other, meaning that the raised exception is matched against each ``raises``'s
case list, and each matching pattern's postcondition must be verified in
conjunction.

.. todo:: Give an example here

.. rubric:: Exemptions

Highly environment dependent exceptions are not expected to be listed, because
they could be unexpectedly triggered depending on the specifics of the machine
the code is executed on.

There are two such exceptions in OCaml: `Stack_overflow` and `Out_of_memory`.
For convenience, these are always assumed to be possibly raised by any
function, without an explicit statement needed. This is equivalent to adding a
``raises Out_of_memory | Stack_overflow -> true`` clause to every function
contract.

Of course, one may still override that behaviour by stating a property whenever
these exceptions are raised, like any other exception.


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
