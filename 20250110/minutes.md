# Gospel type-checker dev meeting minutes 2025-01-10

## [#433](https://github.com/ocaml-gospel/gospel/pull/433) Remove and replace the coercion mechanism

The PR demonstrate that this is now technically feasible to remove the coercion
mechanism from Gospel type-checker, module some hackish modification of the
tests so that we access model fields where there were some coercions.

Merging this PR is postponed until we find a reasonable way to write
specifications without coercions.

## A new release for Gospel

A new release would contain the removal of the distinction between `bool` and
`prop`. As we will want to add it back, the release is postponed.

## Rewriting the type-checker

Technical debt is beginning to slow down the development.

Tiago notes that some documentation of the present code would already be
helpful.

A rewriting could simplify the code base, be the occasion to explicitly lay
down some design choice.

Nicolas will open a document where we can be discussed at the root of the git
repository.

## Additional topic

We've discussed Tiago's new syntax proposition from his mail form the 2024-12-17.
There seems to be a consensus to adopt it.

Nicolas recall the problem that we still can't express directly "if P then we
*must* raise exception E", which would be useful (but not necessary) for how
Ortac/QCheck-STM handle exceptional *vs* normal behaviour.
