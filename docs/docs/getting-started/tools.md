---
sidebar_position: 3
---

# Now what?

You've written your first specification. Now what can you do with it?

Well, your specification alone is already helpful, as it completes the
docstring, which may be incomplete or ambiguous, leading to wrong
interpretations of your semantics, or wrong usage of your library.

But besides the `gospel` binary, we also provide a developer API which lets
other tools leverage these specifications to provide different features. Such
tools already exist, and let you benefit from the specification to bring more
guarantees to your programs.

## Cameleer

Cameleer is a tool for deductive verification of OCaml code.

It extends Gospel to implementation files, where you may add logical annotations
like logical assertions, loop invariants, or termination arguments. The
verification relies on the [Why3](https://why3.lri.fr) framework: `cameleer`
translates the OCaml code into an equivalent WhyML program. It then lets you
analyse this program within the framework (and its IDE!) to prove the
assertions via semi-automated techniques based on SMT provers.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/cameleer).

## Ortac

Ortac is a runtime verification tool for OCaml programs.

It reads the Gospel annotations in the interfaces and generates code that
automatically checks them at runtime. It is implementation-agnostic and quite
flexible: you may use it to trigger exceptions when violations occur, monitor
your program execution by logging unexpected events, or generate testing suites
and fuzzers.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/ortac).

## Why3gospel

Why3gospel is a [Why3](https://why3.lri.fr) plugin that lets you verify that a
program proof refines the Gospel specifications before extracting it to OCaml.

It interfaces the Why3 framework with the Gospel specifications to ensure that
the former refines the latter, guaranteeing that OCaml programs extracted from
proved WhyML indeed comply with their Gospel specification.

For more information, please visit the project page [on
Github](https://github.com/ocaml-gospel/why3gospel).
