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
valid syntax, with the same semantic.

Action items:

- Mário asked Nicolas if it would be possible to have some examples of
  specification with the new syntax and logical library that Ortac could
  consume. Ortac is not updated to the new Gospel yet (WIP), so that will be a
  judgement call. Nicolas will try to write some examples in the next weeks.
- Tiago PR #454 will be closed
- New PRs will be created with the bugfixes contained in #454
