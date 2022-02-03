# Changes and their justification (and problems) wrt version 1

This version of the document doesn't take into account `tast` and `uast`.
It is about the changes in `tterm`, `ttypes`, and `symbols`.

## Remove `Tnot`, `Tbinop` and make `Tapp` taking a tterm rather than a `lsymbol`

The initial design comes from Why3 where there is no higher order.
The new design give a cleaner AST. Adding higher order is closer to OCaml,
though it can bring its problems and complexity.

External tools that does not support higher order can still focus on
a subset of the language.

We should still determine if the gain in expressivity is worth it.

Negation and binary operators will be defined via builtin symbols.

Concerning `Tapp`, this means that to apply a program function, we don't have
to create a node `Tapp (fsapply, [args...])` anymore, which is more comfortable
for the user pov.

## flatten symbols

- `vsymbol`: for values, ghost or not
- `lsymbol`: for functions and predicates, always ghost and has always a type (not on option, as in V1)
- `msymbol`: for model/field (could also be called `fsymbol`), ghost or not
- `csymbol`: for constructors, ghost or not

The two last ones were `lsymbol` in V1, the information being coded in two boolean fields.

- `xsymbol`, same as in V1

This way, we can specify exactly which kind of symbol is allowed in the AST's nodes.
When necessary, we use some polymorphic variants to specify a subset of all the symbols.

## types representation

We removed the `tysymbol` and renamed the `tvsymbol` to `tsymbol`.
The `tysymbol` was keeping information about type constructors (their arguments and retunred type). 

- add an arrow type
- rename Tyapp to Tyconstr as in OCaml; also it takes a `tsymbol` rather than a `tysymbol`
- add Tytuple
- add Tyalias (in V1, this information was in the `tysymbol`, which we don't have anymore

/!\ we should be carefull during typechecking to keep a way to find the type definition
from a `ty`/`tsymbol`

## Remove `TLambda` from `quant`

Add a `Tfun` for anonymous function definition in `tterm`.

## Remove `Ttrue` and `Tfalse` from `tterm`

Those become some builtin symbols.

## ghost information is now stored in the symbol rather than in a label

So now, we have the same `arg_label` than in OCaml.

## Misc

- add `Ttuple` in `term_node`
- add `Ptuple`, `Prec`, `Ptuple` and rename `Papp` to `Pconstr` in `pattern_node`

## Question

- in `pattern`, there is a field `p_vars`, I'm not sure to understand what is it for.
  The set of variables is computed anymay (`Tterm_helper.p_vars`) and it seems the only
  place we access this field is in `tterm_helper` to build patterns. 
  (been removed from v1 in PR #139)
  
- typing use `dterm`. it seems to be based on Why3, I can't find the analog in OCaml.
  Do we need dterms ?
