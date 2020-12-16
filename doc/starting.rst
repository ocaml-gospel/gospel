Starting Example
================

.. code-block:: ocaml

   type 'a t
   (*@ mutable model view: 'a seq *)

Foo bar.

.. code-block:: ocaml

   val push: 'a -> 'a t -> unit
   (*@ push v q
       modifies q
       ensures  q.view = Seq.cons v (old q.view) *)

GOSPEL type-checker
~~~~~~~~~~~~~~~~~~~

.. todo::

   - show how the type-checker works for this example
   - should we also show the use of the `why3gospel` plugin for this example?

Cameleer
~~~~~~~~

Cameleer is a tool for the deductive verification for OCaml code. It takes as
input an OCaml programm, annotated with GOSPEL specification, and translates it
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
