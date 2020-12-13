.. GOSPEL documentation master file, created by
   sphinx-quickstart on Fri Nov 20 09:43:47 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to GOSPEL's documentation!
==================================

:Authors:
  ...
  ...

.. toctree::
   :maxdepth: 2

   starting example, tutorial-like
     which example? queue (cf FM'19 paper)
     quick demo of one or two tools

   language specification
     lexical conventions
     terms and predicates
     type annotations
       (mutable) model
       invariant
     function contracts
       requires/ensures/etc.
       ghost arguments and returned values
     ghost declarations
       (*@ type
       (*@ val
     function/predicate/axiom
     modules and functors
       (*@ with ... *)

     future work: annotating an OCaml implementation
       contract for toplevel let
       ghost arguments and returned values
       assertions
       loop invariants
       contracts for local functions
       ghost code


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
