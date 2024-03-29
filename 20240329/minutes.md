Presents: Tiago, François, Nicolas

François' remarks pre-meeting:

 >    - it has been said in the mathematical library meeting that gospel
will
 > move away from the disctinction between `bool` and `prop`. Should we
tackle
 > this in a near future?

Indeed, there was an apparent consensus to move in this direction, on the
basis that the bool/prop distinction can be difficult to understand for
users
who are not experts in Coq (and can be troublesome even for users who are
experts). In fact, I believe that Why3 does not make this distinction
visible
to the Why3 user. (The distinction may exist internally and/or may be
recreated when exporting proof obligations to Coq.) Perhaps our Why3
experts could comment?

So, if we are able to get away with it, I would suggest living without this
distinction in Gospel. This means that we must somehow manage to recreate
this distinction when translating Gospel to Coq (possibly by decorating the
Coq code with coercions from bool to Prop and/or from Prop to bool).

If we are not able to get away with it, then we can make the distinction
visible in Gospel, but I would like to know exactly what the reasons are.

 >    - There has been some discussion and experimentation about having
 > only one unnamed logical model for an OCaml type. What's the status
 > of this decision?

My impression is that we might be able to agree on the following:

  - by default, there is just one (unnamed) logical model per type,
    so the user does not need to write `view` everywhere

  - if the user declares multiple named models, then this is sugar
    for a single Gospel model, which is a Gospel record type;

  - if any of the named models is "ephemeral" or "mutable"
    (I don't remember what the keyword is!)
    then the whole record model is "ephemeral" as well.
    so we abandon the possibility of having an immutable model
    and a mutable model side by side
    (which could be useful in some cases, e.g.
     for array length and array content)

  - we can hope to remove this restriction in the future.
    We could allow multiple named models as a primitive feature
    and adopt the convention that if there is only one model
    then it does not need a name at all.
    My gut feeling is that it is better to start simple.


- Discussion about [#389](https://github.com/ocaml-gospel/gospel/pull/389)
  adding paths to the Gospel identifiers: We'll adopt the fully resolved paths.

- Discussion about [#390](https://github.com/ocaml-gospel/gospel/pull/390)
  bringing implementation's specification to upstream gospel.

Thanks again for the huge amout of work! Gospel used by Cameleer is now up to
date with the "official" one. All tests are green. The PR need some git history
managment to be reviewedand merged.

Tiago makes the remark than stopping at the untyped ast bring some limitations
for Cameleer, so at some point we may want to type-check this part of the ast
too.

There have already been some discussion in the past about rewriting the gospel
type-checker.

- Discussion about the disctinction between bool and prop.

Tiago already has some code that goes in this direction. He will clean it up
and propose a PR. Thanks!

This should simplify the type-checker a bit and open the way to removing the
coercions.

- discussion about the unammed model

It appears there is still some uncertainty about the whole design.

We should discuss some written examples. Maybe in an issue?

The plan is then to tackle the following things in this order:

1. bool/prop
2. coercion
2. unnamed model

- Finally, a new release of gospel will soon appear.
Maybe just waiting for
- [#386](https://github.com/ocaml-gospel/gospel/pull/386)
- and [#387](https://github.com/ocaml-gospel/gospel/pull/387)
to be merged as they are really small bugfixes
