# exceptions specification

Reminder of the concern: when a function may raise an exception, we have to
express in the normal postcondition that the conditions under which the
exception is raised should not happen (imply false). This works but is a bit
heavy and cumbersome at first sight.

François Pottier made some syntax proposition.

- for function that doesn't raise any exception:

```
assume P; let y = f x in assert Q
```

- for function that may raise an exception:
```
assume P; match f x with y -> assert Q | exception E -> assert Q'
```

This is closer to the OCaml syntax. But, we still have to express in `Q` that
the conditions of the exception don't hold. We need to inspect the argument to
tell in which case we are. Hence another proposition made by François:

```
if is_empty q then match pop q with exception E -> ... else let r = pop q in assert(...)
```

But here again, it should be completed as

```
if is_empty q then match pop q with exception E -> ... | _ -> false else let r = pop q in assert(...)
```

We'll continue to explore these ideas.

# open ml files

Mário Pereira has the code to parse implementation files and type-check the
type signature. Cameleer relies on Why3 to type-check the OCaml code. He will
begin to work on integrating this code to the Gospel repository.

The plan it to have
- Gospel parse `mli` and `ml` files (source -> Parsetree with gospel untyped
  attributes -> Gospel untyped ast)
- Gospel type-checks the interface parts (Gospel untyped ast -> Gospel with
  typed signatures and untyped structures -- except for `Pstr_modetype`) Tools
  that only relies on signatures specification will just ignore the
  implementation parts. Cameleer will still relies on Why3 for type-checking
  the implementation code.

The result will be an ast with a mix of typed and untyped gospel
specifications, but there may be a clear enough delimitation (interface /
implementation).


## Extra discussion

In order to simplify the Gospel source preprocessor, it has been discussed to
remove the nested gospel attributes. These occur for example when a
specification is given to a Gospel logical
function.

For now, it is written:

```
(*@ function f (i : integer) : integer *)
(*@ ensures ... *)
```

The source preprocessor has then to figure out the the second special comment
is to be turned into a gospel attribute attached to the previous gospel
attributes.

I made the prosition to write it in one signel special comment:

```
(*@ function f (i : integer) : integer
    ensures ... *)
```

I'll raise an issue oin github for further discussion.
