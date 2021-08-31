Language Specification
======================

Specification locations
-----------------------

General conventions
^^^^^^^^^^^^^^^^^^^

Gospel annotations are written in interface files (``.mli``).

`OCaml attributes <https://caml.inria.fr/pub/docs/manual-ocaml/attributes.html>`_
with the identifier ``gospel`` are used to bear the Gospel specifications in their
payload, as strings: ``[@@gospel "<spec>"]`` or ``[@@@gospel "<spec>"]``.

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

.. rubric:: Specification of ghost and logical declarations

When ghost and logical declarations need to be specified with a contract, the
contract should reside in an attribute attached to the string containing the
declaration::

  [@@@gospel "val f : int -> int"
    [@@gospel "y = f x ensures x > 0"]]

Gospel preprocessor
^^^^^^^^^^^^^^^^^^^

Writing attributes can be tedious, especially when nested. Gospel
automatically applies a preprocessor that lets you write Gospel
specifications in special comments, starting with a ``@``::

  val f: int -> int           (* An OCaml value declaration *)
  (*@ y = f x                 (* Its Gospel specification   *)
      ensures x > 0 *)

  (*@ type t *)               (* A ghost type declaration   *)
  (*@ model size: int *)      (* Its Gospel specification   *)

This notation will be used throughout the documentation, in place of the
attribute-based one.


Lexical Conventions
-------------------

Gospel uses the `OCaml lexical conventions <https://caml.inria.fr/pub/docs/manual-ocaml/lex.html>`_, with the following
exceptions:

* there are reserved keywords, namely ``checks``, ``consumes``,
  ``diverges``, ``ephemeral``, ``equivalent``, ``model``,
  ``modifies``, ``pure``, ``raises``, ``requires``, ``variant``,
  ``axiom``, ``coercion``, ``ensures``, ``exists``, ``forall``,
  ``invariant``, ``predicate`` ;

* there are a few reserved symbols, namely ``<->``, ``/\``, ``\/``.

In the following, ``lident`` (resp. ``uident``) stands for an
identifier with a lowercase (resp. uppercase) first character.

Terms and Formulas
------------------

Gospel makes a distinction between terms (e.g. ``x+1``) and formulas
(e.g. ``forall i. f i > 0``). That distinction is made during type
checking, and not at the syntax level. Below, ``expr`` stands for a
Gospel expression, be it a term or a formula.

.. rubric:: Type expressions

Type expressions follow the OCaml syntax.

.. productionlist::
   typexpr: `lname`
        : | "'" `lident`
        : | "(" `typexpr` ")"
        : | `typexpr` `lpath`
        : | "(" `typexpr` ("," `typexpr`)+ ")" `lpath`
        : | `typexpr` ("," `typexpr`)*
        : | ("?"? `lident` ":")? `typexpr` "->" `typexpr`
        : | `typexpr` ("*" `typexpr`)+
   lpath: (`upath` ".")? `lident`
   upath: `uident`
        : | `upath` "." `uident`

.. rubric:: Expressions

A large fragment of the OCaml syntax is reused for Gospel expressions.

.. productionlist::
    expr: `constant`
        : | (`upath` ".")? `ident`
        : | "(" `expr` ")"
        : | "(" `expr` ("," `expr`)+ ")"
        : | `expr` "." "(" `expr` ")"
        : | `expr` `infix_op` `expr`
        : | `prefix_op` `expr`
        : | "not" `expr`
        : | `expr` `expr`+
        : | "if" `expr` "then" `expr` "else" `expr`
        : | "let" `pattern` "=" `expr` "in" `expr`
        : | "match" `expr` ("," `expr`)* "with" `match_cases`
        : | "fun" `binders` "->" `expr`
        : | `expr` ":" `typexpr`
        : | "{" `fields` "}"
        : | "{" `expr` "with` `fields` "}"
        : | "[@...]" `expr`
        : | ...
   binders: `lident`+ (":" `typexpr`)?
   pattern: "_"
        : | `lident`
        : | `uname` `pattern`?
        : | "()"
        : | "(" `pattern` ")"
        : | `pattern` ("," `pattern`)+
        : | `pattern` "::" `pattern`
        : | `pattern` "as` `lident`
        : | `pattern` "|" `pattern`
        : | "{" `field_pattern_` (";" `field_pattern)* "}"
   field_pattern: `lname` "=" `pattern`
        : | `lname`
   match_cases: "|"? `match_case` ("|" `match_case`)*
   match_case: `pattern` "->" `expr`
   fields: `field` (";" `field`)*
   field: `lname` "=" `expr`
        : | `lname`
   constant: `integer_literal`
        : | `real_literal`
        : | "true" | "false"
        : | "()"
        : | "[]"

.. rubric:: Gospel-specific expressions

In addition, there is syntax that is specific to Gospel.

.. productionlist::
   expr : ...
        : | `expr` "/\" `expr`
        : | `expr` "\/" `expr`
        : | "old" `expr`
        : | `quantifier` `binders` ("," `binders`)* "." `expr`
        : | `expr` "[" `expr` "]"
        : | `expr` "[" `expr` "<-" `expr` "]"
        : | `expr` "[" `expr` ".." `expr` "]"
        : | `expr` "[" ".." `expr` "]"
        : | `expr` "[" `expr` ".." "]"
        : | `expr` "." "(" `expr` "<-" `expr` ")"
   quantifier: `forall`
        : | `exists`

Note that ``e1[e2]`` is part of the OCaml syntax (application
of ``e1`` to a single-element list ``[e2]``) but is listed here as it
has a different meaning in Gospel (namely, access to a sequence element).

There are two operators for logical conjunction, namely ``&&`` and
``/\``, and two operators for logical disjunction, namely ``||`` and
``\/``. A distinction between the two, if any, is tool-specific. For
instance, a deductive verification tool may interpret ``A && B`` as
``A /\ (A -> B)`` and a runtime assertion checking tool may interpret
``A && B`` as a lazy operator (as in OCaml) and ``A /\ B`` as a strict
operator.

A noticeable difference w.r.t. the OCaml syntax is that infix
operators can be chained in Gospel. One can write ``0 <= n < 100``,
for instance, and it is interpreted as ``0 <= n /\ n < 100``.


Function Contracts
------------------

An OCaml function is given a formal specification by appending one Gospel
function contract to its declaration. Here is an example::

  val eucl_division: int -> int -> int * int
  (*@ q, r = eucl_division x y
      requires y > 0
      ensures  x = q * y + r
      ensures  0 <= r < y *)

Such a contract is composed of two parts:
 - The first line is the header of the contract; it names the function arguments
   and result. It must appear at the beginning of the contract.
 - The next lines contain as many specification `clauses` as needed. Here we
   have three clauses: one :ref:`precondition <Preconditions>` introduced with
   ``requires``, and two :ref:`postconditions <Postconditions>` introduced with
   ``ensures``.

.. productionlist::
    contract: `header` `clause`*
    header: (`ident_tuple` "=")? `identifier` `parameter`+
    clause: `precondition`
        : | `postcondition`
        : | `exceptional_postcondition`
        : | "modifies" `expr` ("," `expr`)*
        : | `equivalence`
        : | `divergence`
        : | "consumes"
    ident_tuple: `identifier` ("," `identifier`)*
    parameter: "()" | `identifier` | "~" `identifier` | "?" `identifier`

.. todo:: ghost parameters and results

.. todo:: contract for a constant e.g. val x: int ensures x > 0

.. note::

   In the absence of a contract attached to a function declaration, **no
   assumptions are made** on the behaviour of the function.

   No preconditions or postconditions are to be verified, and the function may
   diverge, raise unlisted exceptions, or modify mutable types, etc.
   However, it cannot break any :ref:`type invariant <Type invariants>`.

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
``Stack_overflow`` and ``Out_of_memory``.

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

Complementary to other specification clauses, Gospel allows the writer of the
interface to talk about *code equivalence* in the function contract. Such a code
equivalence is specified in a clause introduced by the keyword ``equivalent``,
followed by a string containing the OCaml code the function should behave like.

.. productionlist::
    equivalence: "equivalent" `string`

This is particularly useful when specifying functions which behaviour can hardly
be expressed in pure logic::

  val iter: ('a -> unit) -> 'a t -> unit
  (*@ iter f t
      equivalent "List.iter f (to_list t)" *)

With such a specification, no logical assertion is provided, but applying
``iter`` to ``f`` and ``t`` is equivalent to applying ``List.iter`` to ``f``,
and the conversion of ``t`` to a list. This does not leak implementation
details, as ``iter`` might in fact be implemented in a different, more
efficient, way, it does however make the specification concise and elegant.

.. todo:: Warn about the OCaml code not being parsed/type-checked at the moment
          (or do it).

.. index:: diverges

Non termination
^^^^^^^^^^^^^^^

OCaml functions with attached contracts are always considered to be terminating
by default.

If one function is allowed to not terminate (e.g. a game or server main loop, a
function waiting for a signal or event, etc.), one can add this information to
the contract using the ``diverges`` keyword.

.. productionlist::
    divergence: "diverges"

The following example states that the execution of the function ``run`` may not
terminate. It is not specified whether this function is always non-terminating
or not::

  val run : unit -> unit
  (*@ run ()
      diverges *)

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

Type invariants
^^^^^^^^^^^^^^^

.. todo:: document type invariants

.. index:: function
.. index:: predicate
.. index:: axiom
.. index:: coercion

Ghost and Logical Declarations
------------------------------

.. todo:: do it
