# DESIGN

This document aims at centralising design and implementation discussions.
Please edit with your remarks.

## AST

Gospel specifications are attached to a *modified* copy of the OCaml AST, using
`Gospel.Identifier.Ident.t` rather than OCaml ones. Attaching Gospel
specifications to the actual OCaml AST (in attributes *e.g*) would made
communication with Odoc easier. This would lead to an easier integration of
Gospel in OCaml compilation pipeline in the future if this is something that
we would want.

`Uast.term_desc` and `Tterm.term_node` are quite different:

- `Uast.term_desc.Ttuple` doesn't have a corresponding node in
  `Tterm.term_node`, leading to the creation of `fs_tuple`s logical symbols.
  Authors of tool consuming Gospel typed AST may want to benefit from pattern
  matching on `Tterm.term_node` rather than relying on the
  `Symbols.is_fs_tuple` function?
- `Uast.term_desc.Trecord` doesn't have a corresponding node in
  `Tterm.term_node`, leading to the creations of identifier of the form
  `constr#type_name`. Building a record is then represented as the application
  of `constr#type_name` to the field's content. The name of the fields are not
  directly accessible (but they have been checked at typing). Overall, this
  design works but may seem a bit exotic.
- `Tterm.term_node.Tapp` takes a logical symbol for the applied function,
  leading to the creation of `fs_apply` symbols (which is turn have to be dealt
  with in the tools consuming Gospel AST and cause #430 for example).

## Type-checker

Type-checker is not modular (#377), meaning each time it encounters an `open`
it will look for the file corresponding to the opened module and type-check it.
This is nice because we don't have to care about dependencies. But this can
have a cost on non trivial projects: if two modules in our project open the
same dependency, typing the project will cause the said dependency to be typed
twice. Gospel already save the typing information in a (`.gospel`) file (#376).
Gospel type-checker could take the saved information as argument on the command
line, avoiding the said complexity.

`Tmodule` should at least be documented in order to understand how the relation
between OCaml and Gospel values and types are handled.

The list of built-in types contains surprising items like:
- format6
- lazy

## Patterns

As for terms, we are lacking tuples and records in the typed patterns.

We have an interval pattern for characters, that has been added with the
pattern matching exhaustivity test. The motivation is not clear as it is done
in a big monolithic commit. Do we want to support pattern matching on intervals
of character?

Gospel support partial function (there are a number of them in the Gospel
standard library). But the exhaustivity check forbid to define them by partial
pattern matching. What are the motivation for the pattern matching exhaustivity check?
(Note: there are some bugs, *e.g* #399, and it makes modifying the pattern
representation more difficult, as in more work).
