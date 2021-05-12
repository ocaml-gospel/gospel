Starting Example
================

As a warmup example, we use Gospel to provide formal specification to a mutable
data structure, specifically a FIFO queue. This example is mainly adapted from
our FM'19 paper.

.. todo::

   references?

First, we shall define the type of queues. Following the OCaml convention, we
use `'a t` to represent a polymorphic queue, storing elements of type `'a`. In
order to be able to reason about the elements of a queue, we attach to its type
declaration a *model field*, as follows:

.. code-block:: ocaml

   type 'a t
   (*@ mutable model view: 'a seq *)

Comments of the form `(*@` introduce a Gospel annotation. Here, model `view`
represents the mathematical sequence of elements stored in the queue. Type `'a
seq` is the type of logical sequences, defined in the Gospel standard library,
and is usable only for specification. Finally, the `mutable` keyword states
the `view` field can be updated in-place, following the mutable nature of the
underlying data structure. This is, in fact, a first manifestation of how Gospel
annotations provide extra insight to the OCaml programmer: the `mutable`
specification rigorously states this is an imperative structure, which could not
be deduced from the definition of type `'a t` alone.

Let us now declare and specify a `push` operation for the queue data
structure. First, we write the OCaml type (function signature) then the Gospel
specification, as follows:

.. code-block:: ocaml

    type 'a t
    (*@ mutable model view: 'a seq *)

    val create : unit -> 'a t
    (** Return a new queue, initially empty. *)
    (*@ q = create ()
          ensures q.view = empty *)

    val push : 'a -> 'a t -> unit
    (** [add x q] adds the element [x] at the end of the queue [q]. *)
    (*@ push x q
          modifies q
          ensures  q.view = snoc (old q.view) x *)

    val pop : 'a t -> 'a
    (** [pop q] removes and returns the first element in queue [q]. *)
    (*@ r = pop q
          checks   q.view <> empty
          modifies q
          ensures  old q.view = cons r q.view *)

    + variant with raises

    val is_empty : 'a t -> bool
    (** Return [true] if the given queue is empty, [false] otherwise. *)
    (*@ b = is_empty q
          ensures b <-> q.view = empty *)

.. todo::

   show all the signature first, or at the end with Gospel spec?

.. code-block:: ocaml

   val push: 'a -> 'a t -> unit
   (*@ push v q
       modifies q
       ensures  q.view = Seq.cons v (old q.view) *)

.. todo::

   should we use `snoc` or `::` ?

The first line of the specification names the two arguments of `push` as `v` and
`q`, respectively. Next, the `modifies` states that, during the execution of
`push`, a side-effect mutates the contents of queue `q`. Finally, the `ensures`
clause introduces a postcondition. In this case, it asserts the `view` sequence
of `q`, after a call to `push`, consists of the same sequence before the call
extended with element `v` added at the front. In Gospel, one uses the keyword
`old` to refer to the pre-state (*i.e.*, before the function call) of some
mutable field.

.. code-block:: ocaml

   exception Empty

   val pop: 'a t -> 'a
   (*@ v = pop q
         raises   Empty -> q.view = empty
         modifies q
         ensures  old q.view = q.view ++ v :: nil *)

Foo

.. todo::

   to explain `raises`: clearly state q.view (implicitly?) refers to the
   pre-state


.. code-block:: ocaml

   val is_empty: 'a t -> bool
   (*@ b = is_empty q
         ensures b <-> q.view = empty *)

.. todo::

   `is_empty` is interesting because it is an effect-free function

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
