Specifications Walkthoughs
==========================

Mutable queues
^^^^^^^^^^^^^^

In this example, we will provide provide formal specification to a mutable
queue data structure. This work is adapted from :cite:`chargueraud:hal-02157484`.

First, we shall define the type of queues. Following the OCaml convention, we
use ``'a t`` to represent a polymorphic queue, storing elements of type ``'a``.
In order to be able to reason about the elements of a queue, we attach one model
to its type declaration::

  type 'a t
  (*@ mutable model view: 'a seq *)

The model ``view`` represents the mathematical sequence of elements stored in
the queue. The type ``'a seq`` is the type of logical sequences, defined in the
Gospel standard library, and is usable only for specification. Finally, the
``mutable`` keyword states that the ``view`` field can change over time. This is
a first manifestation of how Gospel annotations provide extra insight to the
OCaml programmer: the ``mutable`` specification states that type ``'a t`` stands
for a mutable data type, which can not be deduced from its declaration alone.

Let us now declare and specify a ``push`` operation for the data structure::

  val push: 'a -> 'a t -> unit
  (*@ push v q
      modifies q
      ensures  q.view = Seq.cons v (old q.view) *)

- The first line of the specification names the two arguments of ``push``, as
  ``v`` and ``q``, respectively.
- Next, the ``modifies`` states that the function ``push`` may mutate the
  contents of ``q``.
- Finally, the ``ensures`` clause introduces a postcondition. In this case, it
  describes the ``view`` model of ``q`` after a call to ``push``: the new
  ``view`` is the extension of the old value of ``view`` with the element ``v``
  at the front. We use the keyword ``old`` to refer to the value of an expression
  (here, ``q.view``) in the pre-state, *i.e.*, before the function call.

Note that the module ``Seq`` is part of the Gospel standard library, and is not
to be confused with module ``Seq`` from the OCaml standard library.

Let us now move to another function, ``pop``. This function raises an ``Empty``
exception if its argument is an empty queue. We specify this behavior as
follows::

  exception Empty

  val pop: 'a t -> 'a
  (*@ v = pop q
      modifies q
      ensures  old q.view = q.view ++ Seq.cons v empty
      raises   Empty -> q.view = old q.view = empty *)*

We have two postconditions:
- The first one, introduced with ``ensures``, states the postcondition that
  holds whenever function ``pop`` returns a value ``v``. As with function
  ``push``, the clause ``modifies`` indicates that ``q`` may be mutated by this
  function call. Note that this applies to the exceptional case as well, and
  that's why we state that ``q`` is both empty and not modified.
- The second one, introduced by ``raises``, states the postcondition that holds
  whenever exception ``Empty`` is raised.

Now, let us consider a scenario where one calls the ``pop`` operation only for
`provably` non-empty queues. For such case, instead of considering the
possibility of raising the ``Empty`` exception, we attach the following
precondition to the function's contract:

.. code-block:: ocaml

   val unsafe_pop: 'a t -> 'a
   (*@ v = pop q
         requires q.view <> empty
         modifies q
         ensures  old q.view = q.view ++ (Seq.cons v empty) *)

The ``requires`` clause introduces a property that must hold whenever one calls
``unsafe_pop``. Following the OCaml tradition, we use the prefix ``unsafe_`` to
clearly state this function is only supposed to be used by clients that are sure
to respect the precondition (for instance, when the client code is itself
formally verified).

Instead of assuming that the precondition is guaranteed by the caller, we can
adopt a more defensive approach where ``pop`` raises ``Invalid_argument`` on an
empty queue. This is idiomatic of OCaml libraries. Gospel provides a way to
declare such a behavior, using ``checks`` instead of ``requires``. In our case,
this is as follows:

.. code-block:: ocaml

   val pop: 'a t -> 'a
   (*@ v = pop q
         checks   q.view <> empty
         modifies q
         ensures  old q.view = q.view ++ (Seq.cons v empty) *)

The meaning of ``checks`` is really that of a `precondition` that is dynamically
checked at function entry. It is up to the implementation of ``pop`` to
guarantee that whenever ``q.view <> empty`` does not hold, the exception
``Invalid_argument`` is raised.

.. todo::
   make a reference to Ortac?

The next function features a very simple specification. Consider the following
declaration for an emptiness test, together with its Gospel contract:

.. code-block:: ocaml

   val is_empty: 'a t -> bool
   (*@ b = is_empty q
         ensures b <-> q.view = empty *)

This function returns the Boolean value ``true`` if and only if the queue is
empty. Such a property is exactly what is captured in the postcondition.
Although very simple, the above specification states an important property: the
argument ``q`` is read-only, hence function ``is_empty`` is effect-free. In
particular, we know that ``q.view`` is not modified after a call to ``is_empty
q``.

Generally speaking, whenever an argument or mutable field is not declared
withing a ``modifies`` clause, then it is treated as a read-only value.

The next function features the creation of a queue. Its OCaml declaration and
Gospel specification are as follows:

.. code-block:: ocaml

    val create: unit -> 'a t
    (*@ q = create ()
          ensures q.view = empty *)

The newly created queue, named ``q``, has no elements, hence its ``view`` model
corresponds to the ``empty`` sequence, exactly as stated in the postcondition.
It is worth mentioning that the Gospel specification implicitly assumes ``q`` to
be disjoint from every previously-allocated queue. This is an important design
choice of Gospel, following the `rule of thumb` that writing a function that
returns a non-fresh, mutable data structure is considered bad practice in OCaml.

Let us conclude this introduction to Gospel with a last function to concatenate
two queues. Several approaches are possible for such a function, and we
illustrate three of them. Let us start with a concatenation that transfers all
elements from one queue to another, with the following specification:

.. code-block:: ocaml

   val in_place_concat: 'a t -> 'a t -> unit
   (*@ in_place_concat q1 q2
         modifies q1, q2
         ensures  q1.view = empty
         ensures  q2.view = old q1.view ++ old q2.view *)

Here, the contract states that both queues are modified. The queue ``q1`` is
emptied (after the call), its elements being appended to the queue ``q2``. Note
the use of ``old`` in the second postcondition.

One could think of a slightly different implementation. ...

.. code-block:: ocaml

   val in_place_destructive_concat: 'a t -> 'a t -> unit
   (*@ in_place_destructive_concat q1 q2
         consumes q1
         modifies q2
         ensures  q2.view = old q1.view ++ old q2.view *)

.. code-block:: ocaml

   val nondestructive_concat: 'a t -> 'a t -> 'a t
   (*@ q3 = nondestructive_concat q1 q2
         ensures q3.view = q1.view ++ q2.view *)
