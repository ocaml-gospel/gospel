# Changes and their justification (and problems) wrt version 1

## Remove `Tnot`, `Tbinop` and make `Tapp` taking a tterm rather than a `lsymbol`

The initial design come from Why3 where there is no higher order.
The new design give a cleaner AST. Adding higher order is closer to OCaml,
though it can bring its problems.

Negation and binary operators are defined via builtin symbols.

Concerning `Tapp`, this means that to apply a program function, we don't have
to create a node `Tapp (fsapply, [args...])` anymore, which is more comfortable
for the user pov.

## Remove `TLambda` from `quant`

Add a `Tfun` for anonymous function definition in `tterm`.

## Remove `Ttrue` and `Tfalse` from `tterm`

Those become some builtin symbols.

## ghost information is now stored in the symbol rather than in a label

So now, we have the same `arg_label` than in OCaml.

## flatten symbols

- `vsymbol`: for values, ghost or not
- `lsymbol`: for functions and predicates, always ghost and has always a type (not on option, as in V1)
- `msymbol`: for model/field (could also be called `fsymbol`), ghost or not
- `csymbol`: for constructors, ghost or not

The two last ones were `lsymbol` in V1, the information being coded in two boolean fields.

- `xsymbol`, same as in V1

## `Ttypes.ty`

we removed the `tysymbol` and renamed the `tvsymbol` to `tsymbol`.

- add an arrow type
- rename Tyapp to Tyconstr as in OCaml; also it takes a `tsymbol` rather than a `tysymbol`
- add Tytuple
- add Tyalias (in V1, this information was in the `tysymbol`, which we don't have anymore

/!\ we should be carefull during typechecking to keep a way to find the type definition
from a `ty`/`tsymbol`

## Misc

- add `Ttuple` in `term_node`
- add `Ptuple`, `Prec`, `Ptuple` and rename `Papp` to `Pconstr` in `pattern_node`
