---
sidebar_position: 3
---

# Expressions

Gospel expression can be either a term (e.g. `x+1`) or a formula (e.g.
`forall i. i > 2 -> f i > 0`). This distinction is made during type checking,
and not at the syntax level.

The syntax for Gospel expressions is largely OCaml's syntax.
The main differences are:

- Gospel can represent the following formulae:

  ```ocaml invalidSyntax
  forall x,y. rest  (* universal quantification of [x] and [y] in [rest] *)
  exists x,y. rest  (* existential quantification of [x] and [y] in [rest] *)
  form1 /\ form2    (* conjunction *)
  form1 \/ form2    (* disjunction *)
  form1 <-> form2   (* equivalence *)
  form1 -> form2    (* implication *)
  ```

- Gospel can represent the following terms:

  ```ocaml invalidSyntax
  old expr          (* value of [expr] before running the function
                       (in a post-condition of the function) *)
  s[i]              (* [i]-th element of the sequence [s] *)
  s[i..j]           (* slice of sequence of [s] from [i] to [j] indices *)
  s[..j]            (* slice of sequence of [s] from beginning to index [j] *)
  s[i..]            (* slice of sequence of [s] from index [i] to end *)
  f[x->v]           (* function equal to [v] on [x] and to [f y] on [y] *)
  ```

Note that `e1[e2]` is part of the OCaml syntax (application of `e1` to a
single-element list `[e2]`) but has a different meaning in Gospel, namely,
access to a sequence element.

There are two operators for logical conjunction, `&&` and `/\`, and two
operators for logical disjunction: `||` and `\/`. A difference between the two,
if any, is tool-specific. For instance, a deductive verification tool may
interpret `A && B` as `A /\ (A -> B)` and a runtime assertion checking tool may
interpret `A && B` as a lazy operator (as in OCaml) and `A /\ B` as a strict
operator.

Another noticeable difference w.r.t. the OCaml syntax is that infix operators
can be chained in Gospel. One can write `0 <= n < 100`, for instance, and it is
interpreted as `0 <= n /\ n < 100`.
