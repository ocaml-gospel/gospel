# Notes on Gospel examples that should be consumable by Ortac/QCheck-STM

Those examples use some addition to the Gospel standard library that I've done
but not merged / placed in a proper PR yet. Namely:

- `decide (p : prop) : bool`
- `fst` and `snd` that were present in the previous version of the Gospel
  stdlib
- `Sequence.is_empty`

Specifcations for the `hashtbl` are still quite verbose. One way to improve
that would be to add some primitives about association sequences in the Gospel
stdlib.

I *think* the specifications would be consumable by Ortac/QCheck-STM. I haven't
checked as I haven't yet update Ortac to the new Gospel.

For a function to be included in the generated tests, its specifications
*should* contain a `ensures` clause describing, in a computable way, the new
state of the datastructure if it is being modified. Description should be of
the form `x = term` where `x` denotes the name of the datastructure in the
argument list and the term can only refer to `x` in the scope of an `old`
operator

If there is a returned value, a postcondition describing it in a computable way
will be used in the generation of the bug report if present.

The other postonconditions will be checked as postconditions in the same way
the QCheck-STM test framework does it. At the time of checking the
postconditions, the test framework has access to the result of the function
being tested (in the form of an `('value, exn) result` if necessary) so that it
can differentiate between normal and exceptional behaviour.

The changes bring by the new version of Gospel are not a problem in writing
specification for dynamic verification. The main strugle is the Gospel standard
library. I had to add some functionalities to be able to write the
specifications.
