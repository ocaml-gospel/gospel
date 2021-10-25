---
sidebar_position: 6
---

# Function contracts

A function contract is a formal Gospel specification attached to the declaration
of an OCaml function in an interface. Here is an example:

```ocaml
val euclidean_division: int -> int -> int * int
(*@ q, r = euclidean_division x y
    requires y > 0
    ensures  x = q * y + r
    ensures  0 <= r < y *)
```

A function contract is composed of two parts:
 - The first line is the **header** of the contract; it names the function
   arguments and result. It must appear at the beginning of the contract.
 - The next lines contain as many specification `clauses` as needed. The
   previous example features three clauses: one pre-condition introduced by
   `requires`, and two post-conditions introduced by `ensures`.

:::caution

   In the absence of a contract attached to a function declaration, you cannot
   make any assumptions about its behaviour.

   Post-conditions may not hold, the function may diverge, raise unlisted
   exceptions, modify mutable states. However, **it still cannot break any type
   invariant.**

   You can still enable the default implicit properties about exceptions, mutability,
   non-termination, etc. by creating an empty contract:

   ```ocaml
   val euclidean_division: int -> int -> int * int
   (*@ q, r = euclidean_division x y *)
   ```

:::

```ebnf title="Function contract syntax"
contract = header? clause*
header = (identifier_tuple "=")? identifier parameter+
clause = "requires" formula
       | "checks" formula
       | "ensures" formula
       | "raises" exception_case ("|" exception-case)*
       | "modifies" expr ("," expr)*
       | "equivalent" string_literal
       | "diverges"
       | "pure"
       | "consumes" expr ("," expr)*
exception_case = qualid "->" formula
               | qualid pattern "->" formula
               | qualid
identifier_tuple = identifier ("," identifier)*
parameter = "()" | identifier | "~" identifier | "?" identifier
```

##  Default behaviour

To avoid boilerplate for usual properties, Gospel applies a default contract
whenever a function has a specification attached. Of course, any
explicitly declared clause overrides this implicit contract.

When a function has a contract attached, the default contract contains the
following properties:
- The function **terminates**.
- The function **does not raise any exception** other than `Stack_overflow` or
  `Out_of_memory`.
- The function **does not have any visible side-effect on the memory**. In other
  words, if it mutates some data, this has no observable influence on the values
  in the rest of the program.

## Pre-conditions

Pre-conditions are **properties that must hold at the function entry**. You can use
them to describe requirements on the function's inputs, but also possibly on a
global state that exists outside of the function arguments.

You can express pre-conditions using the keyword `requires` or `checks`, followed by a
formula.

### `requires`

A `requires` clause states under which conditions a specified function has a
well-specified behaviour.

Whenever a `requires` pre-condition is violated, its behaviour becomes
unspecified, and the call should be considered faulty. Even if the function
execution terminates, any other information provided by the contract
(post-conditions, exceptions, effects, ...) cannot be assumed.

For example, the following function requires `y` to be positive to behave correctly.

```ocaml {3}
val eucl_division: int -> int -> int * int
(*@ q, r = eucl_division x y
    requires y > 0
    ensures  x = q * y + r
    ensures  0 <= r < y *)
```

### `checks`

Pre-conditions introduced with `checks` hold at function entry. However, unlike
`requires` clauses, the behaviour of the function is well specified in case the
pre-state does not meet such a pre-condition: the function fails by raising an
OCaml `Invalid_argument` exception, and does not modify any existing state. The
call is not faulty, but the caller is now in charge of handling the exception.

For example, if we change the function contract of `eucl_division`
above by replacing `requires` with `checks`, it now states that the
function raises `Invalid_argument` whenever `y <= 0`.

:::note Combining multiple pre-conditions


Whenever multiple pre-conditions of the same kind coexist, they hold as a
conjunction, which means

```ocaml
(*@ ...
    requires P
    requires Q *)
```
is equivalent to:

```ocaml
(*@ ...
    requires P /\ Q *)
```

When combining `checks` and `requires` pre-conditions, the declaration order
does not matter. The `requires` clauses take precedence and must always be
respected; otherwise the `checks` behaviour cannot be assumed. This means that
ultimately,

```ocaml
(*@ ...
    requires P
    checks Q *)
```
is equivalent to:

```ocaml
(*@ ...
    requires P
    checks P -> Q *)
```
:::

:::tip

Specification formulas can often be written using few clauses, but splitting the
specification into several smaller clauses leads to better readability and
maintainability and is therefore encouraged.

:::


## Post-conditions

Post-Conditions are **properties that hold at the function exit**. They are used
to specify how the function's outputs relate to its inputs and how the call
mutated the memory.

:::caution

  **When a function raises an exception, its post-conditions are not expected to
  hold.** You must use exceptional post-conditions instead.

:::

Gospel introduces post-conditions using the keyword `ensures`, followed by a
formula.

As discussed in the previous section, the property expressed by the formula is
verified after the function call only if the pre-conditions were satisfied.

:::note Combining multiple post-conditions

The handling of multiple post-conditions is identical to pre-conditions of the
same kind: multiple post-conditions are merged as a conjunction:

```ocaml
(*@ ...
    ensures P
    ensures Q *)
```

is equivalent to:

```ocaml
(*@ ...
    ensures P /\ Q *)
```

:::

## Exceptional post-conditions

Exceptional post-conditions are used to specify the exceptions that can be
raised by the function, and what post-conditions hold in those cases.

By default, functions should not raise any exceptions, and doing so is a
violation of the specification. Whenever a function can raise an exception as
part of its expected behaviour, this exception must be listed, along with the
associated post-conditions.

:::info

Some exceptions are implicitly allowed and do not have to be listed, because
they could be unexpectedly triggered depending on the specifics of the machine
the code is executed on.

**The implicitly allowed exceptions are `Stack_overflow` and `Out_of_memory`.**

This is equivalent to adding a `raises Out_of_memory | Stack_overflow -> true`
clause to every function contract. Of course, you can still override that
behaviour by stating a property whenever these exceptions are raised, like any
other exception:

```ocaml
(*@ ...
    raises Stack_overflow -> false *)
```

:::

Exceptional clauses are expressed using a `raises` keyword, followed by a list
of cases associating each exception with its formula, with a syntax similar to
pattern-matching.

Gospel expects each `raises` clause to perform an exhaustive pattern matching
for each exception constructor listed in this clause. Similarly to OCaml's
pattern-matching, when an exception is raised, the post-condition that is
satisfied is the first match in the list of the cases.

```ocaml
(*@ ...
    raises Unix_error (ENAMETOOLONG, _, _) -> P
         | Unix_error _                    -> Q *)
```

In the previous contract (notice that it is an exhaustive pattern-matching on
the `Unix_error` exception) only states that `P` holds whenever `Unix_error` is
raised with argument `ENAMETOOLONG`, and that `Q` holds whenever the function
raises `Unix_error` with a different argument (`P` does not necessarily hold in
this case).

:::note Combining multiple exceptional post-conditions

When multiple exceptional post-conditions exist, they hold independently of
each other, meaning that the raised exception is matched against each `raises`'s
case list, and each matching post-condition must hold in conjunction. For
instance, the contract:

```ocaml
(*@ ...
    raises Error "foo" -> P | Error _ -> Q
    raises Error x -> R *)
```

implies that
 - when `Error "foo"` is raised, both `P` and `R` hold, but not necessarily `Q`;
 - when `Error` is raised with with an argument different from `"foo"`, both `Q`
   and `R` hold, but not necessarily `P`.

:::


## Code equivalence

Complementary to other specification clauses, Gospel allows you to talk about
*code equivalence* in the function contract. It consists in a string containing
the OCaml code the function behaves like, preceded by the `equivalent` keyword.

This is useful when specifying functions whose behaviour can hardly be expressed
in pure logic:

```ocaml
val iter : ('a -> unit) -> 'a t -> unit
(*@ iter f t
    equivalent "List.iter f (to_list t)" *)
```

With such a specification, no logical assertion is provided, but applying `iter`
to `f` and `t` is equivalent to applying `List.iter` to `f`, and the conversion
of `t` to a list. This does not leak implementation details, as `iter` might in
fact be implemented in a different, more efficient way. It does however make the
specification concise and elegant.


:::danger

At the moment, the Gospel type-checker does **not** type-check the code provided
inside the `equivalent` clauses, and will take it as-is.

:::

## Non termination

By default, OCaml functions with an attached contract implicitly terminate.

If a function is allowed to not terminate (e.g. a server main loop, a function
waiting for a signal or event, etc.), one can add this information to the
contract using the `diverges` keyword.

The following example states that the execution of the function `run` may not
terminate. It does not specify whether this function is always non-terminating
or not.

```ocaml
val run : unit -> unit
(*@ run ()
    diverges *)
```

## Data mutability

In the default specification, functions do not mutate any observable data. If
your function mutates an argument or some global state, you may specify it using
the keyword `modifies`, followed by an identifier. In the following, the
`contents` model of `a` can be modified by `inplace_map`.

```ocaml {3}
val inplace_map : ('a -> 'a) -> 'a t -> unit
(*@ inplace_map f a
    modifies a.contents *)
```

If the function only modifies a few models of a value, these may be explicitly
added to the clause.


If a specific model is not mentioned, the whole data structure and its mutable
models are potentially mutated.

```ocaml {3}
val inplace_map : ('a -> 'a) -> 'a t -> unit
(*@ inplace_map f a
    modifies a *)
```

In this example, all the mutable models of `a` can be mutated by `inplace_map`.

:::note

When a `modifies` clause is present, it affects all the declared post-conditions
and exceptional post-conditions, meaning that the function may mutate data even
in the case of exceptional post-conditions.

If your data was not mutated in an exceptional post-state, for instance if the
function raised an exception **instead** of mutating the data, you have to
manually specify it:

```ocaml {4}
val inplace_map : ('a -> 'a) -> 'a t -> unit
(*@ inplace_map f a
    modifies a.contents
    raises E -> a.contents = old (a.contents) *)
```

:::

## Pure functions

An OCaml function can be declared as `pure`, which means
- it has no side effect;
- it raises no exception;
- it terminates.

```ocaml {2}
val length : 'a t -> int
(*@ pure *)
```

Pure functions can be used in further Gospel specifications.
On the contrary, OCaml functions not declared as `pure` cannot be used
in specifications.

## Data consumption

Gospel provides a specific syntax to specify that some data has been consumed by
the function, and should be considered dirty — that is, not be used anymore — in
the rest of the program. This is expressed with the `consumes`
keyword:

```ocaml
val destructive_transfer: 'a t -> 'a t -> unit
(*@ destructive_transfer src dst
    consumes src
    ... *)
```


## Ghost parameters

Functions can take or return ghost values to ease the writing of function
contracts. Such values appear within brackets in the contract header.

Consider the following `log2` function:

```ocaml
val log2: int -> int
(*@ r = log2 [i: integer] x
    requires i >= 0
    requires x = pow 2 i
    ensures r = i *)
```

In this contract, the ghost parameter `i` is used in both the pre- and
post-conditions. By introducing it as a ghost value, we avoid using quantifiers to
state the existence of `i`.

:::note

Since the type of ghost parameters does not appear in the OCaml signature, it
must be given explicitly.

:::
