# Gospel type-checker dev meeting minutes 2025-10-24

Attending: Tiago Soares, Mário Perreira, Nicolas Osborne

Agenda: discuss let* and let+ syntax proposition

let* and let+ are predicates that state respectively that the option value is a
Some and the sequence is not empty.

We don't have this information from Option.get and Sequence.uncons functions
(when called out of domain they return an arbitrary value so that we can't
conclude anything).

As stated in previous online discussion, here let* and let+ don't have the
expected semantic from an OCaml programer PoV which may generate confusion. As
they are defined as functions, maybe it could be possible to just use other
names.

One motivation is to aleviate the removal of pattern matching, but another is
the need for less verbose specifcations (no need to state every absurd cases).
Note that it is also important to keep the possibility to be verbose (very
useful for newcomers learning how to write specifications).

Mário proposes another possible syntax:

```ocaml
val f : t -> t option
(*@ r = f t
    ensures let None = r in P t     (* r = None ∧ P t *)
    ensures let Some x = r in Q x t (* ∃ x. r = Some x ∧ Q x t *)
*)
```

Nicolas is not sure how to use this specification in Ortac. But it is OCaml
valid syntax, with the same semantics.

Tiago notes that this is not exactly the same as an OCaml
specification since `None` and `Some` are functions and not ADT
constructors.  Furthermore, it is unclear how we differentiate between
functions the used in the pattern and the fresh variables the pattern
binds.  In this case it is simple since the functions we use in the
patterns start with uppercase letters, however it can get confusing if
we use functions with lowercase letters.  For instance:

``` ocaml
let s : 'a sequence = ... in
let cons x empty = s in x > 0
```

In this term, we are trying to state that the sequence `s` is equal to a
singleton sequence `[x]` where `x` is greater than `0`.  However, how do we
know that `empty` refers to the empty sequence and is not another
variable the user wants bind in this pattern?  There seem to be
a two options:

- Any variable that is not capitalized is bound by the pattern.
  Although it makes this more inline with OCaml since constructors
  must be uppercase, it removes expressivity and forces users to
  capitalize any function they may want to use with this pattern
  matching.  For instance, in the previous example, we would have to
  rename our `cons` and `empty` functions to be uppercase.

- Any variable that is not in scope is considered to be bound by the
  pattern.  Although this gives more expressivity, it would be a bit
  awkward since users could accidentally use variables that are in
  scope in patterns unknowingly that could result in strange terms.
  For instance, if in the previous example there was a value in scope
  named `x` of type `integer`, then the term would still type check but
  would have a different meaning: instead of saying "there exists some
  `x` where `s = cons x empty`" it states "`s = cons x empty`" without
  binding a new name.

Action items:

- Mário asked Nicolas if it would be possible to have some examples of
  specification with the new syntax and logical library that Ortac could
  consume. Ortac is not updated to the new Gospel yet (WIP), so that will be a
  judgement call. Nicolas will try to write some examples in the next weeks.
- Tiago PR #454 will be closed
- New PRs will be created with the bugfixes contained in #454
