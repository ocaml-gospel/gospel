---
sidebar_position: 3
---

# Now What?

You've written your first specification. Now what can you do with it?

Your specification alone is already helpful, as it completes the
docstring, which may be incomplete or ambiguous. That could lead to incorrect
interpretations of your semantics, or wrong usage of your library.

Besides the `gospel` binary, we also provide a developer API which lets other
tools leverage these specifications to provide different features. Such tools
already exist and let you benefit from the specification, which brings more
guarantees to your programs.

## Cameleer

Cameleer is a tool for deductive verification of OCaml code.

It extends Gospel to implementation files, where you may add logical annotations
like logical assertions, loop invariants, or termination arguments. The
verification relies on the [Why3](https://why3.lri.fr) framework. Cameleer
translates the OCaml code into an equivalent WhyML program. It then lets you
analyse this program within the framework (and its IDE!) to prove the
assertions via semi-automated techniques based on SMT provers.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/cameleer).

## Ortac

Ortac is a testing tool for OCaml programs.

It reads the Gospel specifications in the interfaces and generates code to test
that they hold at runtime. It comes with a couple of plugins that provide
different ways to run those tests. You may use it to generate testing suites and
fuzzers or monitor your program execution.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/ortac).

## Why3gospel

Why3gospel is a [Why3](https://why3.lri.fr) plugin that lets you verify that a
program proof refines the Gospel specifications before extracting it to OCaml.

It interfaces the Why3 framework with the Gospel specifications to ensure that
the former refines the latter and to guarantee that OCaml programs extracted
from proved WhyML comply with their Gospel specification.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/why3gospel).
