# Changes and their justification (and problems) wrt version 1

This version of the document doesn't take into account `tast` and `uast`.
It is about the changes in `tterm`, `ttypes`, and `symbols`.

## Remove `Tnot`, `Tbinop` and make `Tapp` taking a `tterm` rather than a `lsymbol`

The initial design comes from Why3 where there is no higher order.
The new design give a cleaner AST. Adding higher order is closer to OCaml,
though it can bring its problems and complexity.
Note that there is a discussion to add higher order to Gospel.

External tools that does not support higher order can still focus on
a subset of the language.

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
The `tysymbol` was keeping information about type constructors (their arguments and returned type). 
Not sure it is the way to go though. We should have somewhere easily accessible the information 
about whether the symbol is the symbol of a type constructor or a "real" type.

In V1:

```ocaml
type ty = { ty_node : ty_node } [@@deriving show]

and ty_node = Tyvar of tvsymbol | Tyapp of tysymbol * ty list
[@@deriving show]

and tysymbol = {
  ts_ident : Ident.t;
  ts_args : tvsymbol list;
  (* we need to keep variables to do things like
     type ('a,'b) t1  type ('a,'b) t2 = ('b,'a) t1 *)
  ts_alias : ty option;
}
```

I propose to have something closer to OCaml.

- add an arrow type
- rename Tyapp to Tyconstr as in OCaml; also it takes a `tsymbol` rather than a `tysymbol`
- add Tytuple
- add Tyalias (in V1, this information was in the `tysymbol`, which we don't have anymore

In V2:

```ocaml
type tsymbol = { ts_name : Identifier.Ident.t }

type ty =
  | Tyvar of tsymbol
  (* alpha types *)
  | Tyarr of arg_label * ty * ty
  (* unary function type. do we want n-ary functions ? *)
  | Tyconstr of tsymbol * ty list
  (* this is equivalent to Ptyp_constr.
     We need a symbol that allows to identify the type constructor *)
  | Tytuple of ty list
  | Tyalias of ty * string
(* this is different from version 1 (coming from Why3).
   we keep the information that a type is an alias at the level of the type representation,
   not the symbol *)
```

**Note**: in recent discussion it seems that there is the idea to really separate program and logic.
If this is what we choose, maybe we can keep OCaml types (`core_type`...) for programs values
and just add logical types?

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

- typing use `dterm`. it seems to be based on Why3, I can't find the analog in OCaml.
  Do we need dterms?
