Little progress on
[issue#366](https://github.com/ocaml-gospel/gospel/issues/366). A lot of
rebasing to do. Rebasing should be done for next meeting, even maybe a PR.
Decision has been taken to add this item for the next release (unlocking
cameleer release).

Regarding the modularisation of the type-checker, the idea doesn't raise any
objections. Decision has been taken to move forward with the `.gospel` file so
that we can focus on the type-checker related set of problems and postpone the
question of the integration with the traditional pipeline. Also, `ortac` can
already benefit from that as dune will decide whether or not to generate the
tests file based on the saved type information rather than the source file
(`.mli`)
[pr#376](https://github.com/ocaml-gospel/gospel/pull/376) can be reviewed.

Mário thinks that there is no need to revise the `Ident` implementation, but
rather be careful about where we plug the typing information read from files in
the namespace. The discussion can continue on the github issue.

Regarding the exceptional postcondition syntax, the provided link was broken,
sorry about that. I'll get my hand again on the examples. Maybe we can move
this discussion to the example group?

Next meeting: Februray the 23rd.
