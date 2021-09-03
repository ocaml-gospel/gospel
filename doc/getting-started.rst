Getting started
===============

.. todo:: Add more links to the language specification, and generally more
          links.

Installing Gospel
^^^^^^^^^^^^^^^^^

Please make sure that you already have a decently recent version ``ocaml`` and
``opam`` installed. Gospel requires the following versions:

- OCaml 4.09.0 or newer
- Opam 2.0 or newer

Please visit `ocaml.org
<https://ocaml.org/learn/tutorials/up_and_running.html>`_ for instructions on
how to get started with your OCaml environment.

Gospel is available on Opam repositories. Installing it is straightforward:

.. code-block:: shell

   $ opam install gospel

This will install the ``gospel`` tool binary, as well as the developer library if you
wish to build your software on top of Gospel. You may check the installation with

.. code-block:: shell

   $ gospel --version
   gospel version 0.1.0

Your first specification
^^^^^^^^^^^^^^^^^^^^^^^^

Let us get started with a simple specification example, and specify a generic
interface for polymorphic, limited capacity containers::

  type 'a t
  (** The type for containers. *)

  exception Full

  val create: int -> t
  (** [create capacity] is an empty container which maximum capacity
      is [capacity]. *)

  val is_empty: 'a t -> bool
  (** [is_empty t] is [true] iff [t] contains no elements. *)

  val clear: 'a t -> unit
  (** [clear t] removes all values in [t]. *)*

  val add: 'a t -> 'a -> unit
  (** [add t x] adds [x] to the container [t], or raises [Full] if
      [t] has reached its maximum capacity. *)

  val remove 'a t -> 'a -> unit
  (** [remove t x] removes [x] from [t], or raises [Not_found] if
      [x] is not in [t]. *)

Gospel specifications live in special comments, starting with the ``@``
character. These comments may be attached to type declarations or value
declarations. They provide a specification for the signature item they are
attached to.

Let's start by specifying the abstract type ``'a t``. As a container with fixed
capacity, we can model it with two bits of information: a fixed integer
capacity, and a set of ``'a`` values, representing its contents. Note that the
capacity is not mutable, while the contents are. This logical modelisation of
the container directly translates into Gospel::

  type 'a t
  (** The type for containers. *)
  (*@ model capacity: int
      model contents: 'a set *)

Notice that documentation comments and Gospel specification can coexist, and
even often help understand each other! However, for the sake of brevity, we will
omit them in the rest of this section.

One may also note that the capacity must be positive, and number of values in
the ``contents`` set may not exceed ``capacity``. Those is a type invariants:

.. code-block::
   :emphasize-lines: 4,5

    type 'a t
    (*@ model capacity: int
        mutable model contents: 'a set
        invariant capacity > 0
        invariant Set.cardinal contents <= capacity *)

Now that our type is annotated with its models and invariants, we can attach
specifications to the functions to show how they interact with the container.

The function ``create`` returns a container when provided a capacity. We may
want to specify three bits of information:

- The provided capacity is positive.
- The capacity of the returned container is indeed the one received as an
  argument.
- The container is empty.

Let's write a Gospel formalisation of that contract. The contract starts with a
header that lets us name the arguments and return value (we will call the
argument ``c`` and the return value ``t``) so we can mention them in the rest of
the specification. The first property is a precondition of the function (we use
the keyword ``requires``), while the second and third ones are postconditions
(the keyword is ``ensures``)::

  val create: int -> t
  (*@ t = create c
      requires c > 0
      ensures t.capacity = c
      ensures t.contents = Set.empty *)

Now on to ``is_empty`` and ``clear``.

``is_empty t`` is true if and only if ``t`` is empty; this is a postcondition.
This function also (hopefully) has no side-effect: it does not modify ``t``, and
does not depend on any internal state. In Gospel's language, this function is
*pure*::

  val is_empty: t -> bool
  (*@ b = is_empty t
      pure
      ensures b <-> t.capacity = Set.empty *)

Clear removes any element in its argument: it is empty after the call.
Obviously, it modifies its argument. More precisely, it modifies the
``contents`` model of its argument::

  val clear: 'a t -> unit
  (*@ clear t
      modifies t.contents
      ensures t.contents = Set.empty *)

Finally, let's specify ``add`` and ``remove``. A first attempt is similar to the
previous examples. In the following, we use Gospel's ``old`` primitive, which
helps up refer to the state of the container prior to the function execution::

  val add: 'a t -> 'a -> unit
  (*@ add t x
      modifies t.contents
      ensures t.contents = Set.add x (old t.contents) *)

  val remove 'a t -> 'a -> unit
  (*@ remove t x
      modifies t.contents
      ensures t.contents = Set.remove x (old t.contents) *)

Notice however that this specification is incomplete. Indeed, one specifity of
those functions is that they can raise exceptions under certain circumpstances,
and their normal behaviour may thus be altered. Let us complete that contract
with this bit of information. If ``add`` raises ``Full``, we can deduce that
``t.contents`` already contains ``t.capacity`` elements. Likewise, if ``remove``
raises ``Not_found``, then the element we're trying to remove was actually not
in ``t.contents``:

.. code-block::
   :emphasize-lines: 5,11

    val add: 'a t -> 'a -> unit
    (*@ add t x
        modifies t.contents
        ensures t.contents = Set.add x (old t.contents)
        raises Full -> Set.cardinal t.contents = t.capacity *)

    val remove 'a t -> 'a -> unit
    (*@ remove t x
        modifies t.contents
        ensures t.contents = Set.remove x (old t.contents)
        raises Not_found -> not Set.mem x (old t.contents) *)

Notice how we did not need to repeat that ``S.cardinal t.contents <=
t.capacity`` in every contract; as a type invariant, this property implicitely
holds in every function's pre-state and post-state.

We're done! Our module interface is fully specified, independently of any
implementation. Let's finish by verifying that these are well typed by calling
Gospel's type-checker:

.. code-block:: shell

   $ gospel check ./container.mli
   OK

Now what?
^^^^^^^^^

You've written your first specfication. Now what can you do with it?

Well, your specification alone is already helpful, as it completes the
docstring, which may be incomplete or ambiguous, leading to wrong
interpretations of your semantics, or wrong usage of your library.

But besides the ``gospel`` binary, we also provide a developer API which lets
other tools leverage these specifications to provide different features. Some
such tools already exist, and let you benefit from the specification to bring
more guarantees to your programs.

.. index:: cameleer
.. index:: why3
.. index:: smt prover

Cameleer
~~~~~~~~

Cameleer is a tool for the deductive verification of OCaml code.

It extents Gospel to implementation files, where you may add logical annotations
like logical assertions, recursion variants, or loop invariants. The
verification relies on the `Why3 framework <https://why3.lri.fr>`_: ``cameleer``
translates the OCaml code into an equivalent WhyML program. It then lets you
analyse this program whithin the framework (and its IDE!) to prove the
assertions via semi-automated techniques based on SMT provers.

For more information, please visit the project page `on Github
<https://github.com/ocaml-gospel/cameleer>`_.

.. index:: ortac
.. index:: runtime assertion checking

Ortac
~~~~~

Ortac is a runtime verification tool for OCaml programs.

It reads the Gospel annotations in the interfaces and generates code that
automatically checks them at runtime. It is implementation-agnostic and quite
flexible: you may use it to trigger exceptions when violations occur, monitor
your program execution by logging unexpected events or generate testing suites
and fuzzers.

For more information, please visit the project page `on Github
<https://github.com/ocaml-gospel/ortac>`_.

.. index:: why3gospel
.. index:: why3
.. index:: smt prover

Why3gospel
~~~~~~~~~~

Why3gospel is a Why3 plugin that lets you verify that Why3 a program proof
refines the Gospel specifications before extracting it to OCaml.

It interfaces the Why3 framework with the Gospel specifications to ensure that
the former refines the latter, guaranteeing that OCaml programs extracted from
proved WhyML indeed comply with their Gospel specification.

For more information, please visit the project page `on Github
<https://github.com/ocaml-gospel/why3gospel>`_.

Getting help
^^^^^^^^^^^^

.. todo:: finish this section

Please feel free to `open a discussion
<https://github.com/ocaml-gospel/gospel/discussions/new>`_ and share your issues
and ideas!
