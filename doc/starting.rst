Starting Example
================

As a warmup example, we use Gospel to provide formal specification to a mutable
queue data structure (adapted from `this FM'19 paper <https://hal.inria.fr/hal-02157484>`_).

First, we shall define the type of queues. Following the OCaml convention, we
use ``'a t`` to represent a polymorphic queue, storing elements of type ``'a``. In
order to be able to reason about the elements of a queue, we attach to its type
declaration a *model field*, as follows:

.. code-block:: ocaml

   type 'a t
   (*@ mutable model view: 'a seq *)

Comments of the form ``(*@`` introduce a Gospel annotation. Here, model ``view``
represents the mathematical sequence of elements stored in the queue. Type ``'a
seq`` is the type of logical sequences, defined in the Gospel standard library,
and is usable only for specification. Finally, the ``mutable`` keyword states
that the ``view`` field can change over time.
This is a first manifestation of how Gospel
annotations provide extra insight to the OCaml programmer: the ``mutable``
specification states that type  ``'a t`` stands for a mutable data type,
which can not be deduced from its declaration alone.

Let us now declare and specify a ``push`` operation for the queue data
structure. First, we write the OCaml type (function signature) then the Gospel
specification, as follows:

.. code-block:: ocaml

   val push: 'a -> 'a t -> unit
   (*@ push v q
         modifies q
         ensures  q.view = Seq.cons v (old q.view) *)

The first line of the specification names the two arguments of
``push``, as ``v`` and ``q``, respectively. Next, the ``modifies``
states that function ``push`` may mutate the contents of
``q``. Finally, the ``ensures`` clause introduces a postcondition. In
this case, it describes the ``view`` model of ``q`` , after a call to
``push``, as the extension of the old value of ``view`` with element
``v`` in front. One uses the keyword ``old`` to refer to the value of
some expression (here, ``q.view``) in the pre-state, *i.e.*, before
the function call.
Note that module ``Seq`` is part of the Gospel standard library, and
is not to be confused with module ``Seq`` from the OCaml standard library.

Let us know move to another function, to pop an element from the queue.
Following the OCaml standard library idioms, this function raises an
``Empty`` exception on an empty queue. We specify this behavior as follows:

.. code-block:: ocaml

   exception Empty

   val pop: 'a t -> 'a
   (*@ v = pop q
         modifies q
         raises   Empty -> q.view = old q.view = empty
         ensures  old q.view = q.view ++ Seq.cons v empty *)

Here, we have two postconditions. The first one, introduced with
``raises``, states the postcondition that holds whenever exception
``Empty`` is raised. The second one, introduced with ``ensures``,
states the postcondition that holds whenever function ``pop`` returns
a value ``v``. As with function ``push``, the clause ``modifies``
indicates that ``q`` may be mutated by this function call. Note that
this applies to the exceptional case as well, and that's why we state
that ``q`` is both empty and not modified.

An alternative would be ...

.. code-block:: ocaml

   val pop: 'a t -> 'a
   (*@ v = pop q
       requires q.view <> empty
       modifies q
       ensures  old q.view = q.view ++ (Seq.cons v empty) *)


.. code-block:: ocaml

   val is_empty: 'a t -> bool
   (*@ b = is_empty q
         ensures b <-> q.view = empty *)

.. todo::

   `is_empty` is interesting because it is an effect-free function

.. code-block:: ocaml

    val create : unit -> 'a t
    (** Return a new queue, initially empty. *)
    (*@ q = create ()
          ensures q.view = empty *)

.. todo::

   `concat`: should we show all the free variants? It could be interesting in
   order to showcase the expressiveness of Gospel to cope with different
   programming scenarios.

Gospel type-checker
~~~~~~~~~~~~~~~~~~~

.. todo::

   - show how the type-checker works for this example
   - should we also show the use of the `why3gospel` plugin for this example?

Cameleer
~~~~~~~~

Cameleer is a tool for the deductive verification for OCaml code. It takes as
input an OCaml programm, annotated with Gospel specification, and translates it
into an equivalent WhyML counterpart.

.. todo::

   include a more comprehensive introduction to the Cameleer tool

Following the Baker's approach, we encode a queue using a pair of lists to store
its elements. We begin by declaring the following type definition:

.. code-block:: ocaml

   type 'a t = {
     mutable front: 'a list;
     mutable rear : 'a list;
     mutable size : int;
     mutable view : 'a list [@ghost];
   } (*@ invariant size = length view *)
     (*@ invariant (front = [] -> rear = []) &&
                   view = front ++ List.rev rear *)

.. todo::

   change the type of field `view` to a sequence

This type is equipped with an invariant...

Simple operations over queues, the `create` and `is_empty` functions, as
follows:

.. code-block:: ocaml

   let create () = {
     front = [];
     rear  = [];
     size  = 0;
     view  = [];
   } (*@ q = create ()
           ensures q.view = [] *)

   let [@logic] is_empty q = q.size = 0
   (*@ b = is_empty q
         ensures b <-> q.view = [] *)

A more interesting function, the `push` ...

.. code-block:: ocaml

   let push x q =
     if is_empty q then q.front <- [x] else q.rear <- x :: q.rear;
     q.size <- q.size + 1;
     q.view <- q.view @ (x :: [])
   (*@ push x q
         ensures q.view = (old q.view) @ [x] *)

.. todo::

   change the `push` operation to follow the queue model of the FM'19 paper

OCaml RTAC
~~~~~~~~~~

.. todo::

   use the ephemeral queue example to showcase Clément's RTAC tool
