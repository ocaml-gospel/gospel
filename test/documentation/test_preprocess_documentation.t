Testing interaction between `gospel pps` and Ocaml compiler. The issue is that
`gospel pps` detached documentation comments. In order to check whether this is
the case or not, we need to make the output of the gospel preprocessor go
through OCaml compiler and then ask for a source that could have generated it
(with the `-dsource` option).

First, we create a small test artifact with the problematic interleaving of
documentation comment and gospel specification:

  $ cat > foo.mli << EOF
  > val f : int -> int
  > (** documentation *)
  > (*@ y = f x *)
  > EOF

Now, we look at how the OCaml compiler understand the output of the gospel
preprocessor. We also enable the compiler warning about unexpected docstring,
and cleanup stderr because the name of the file in the error message will
change at each build. Note that this cleanup relies on the number of lines of
the interface `foo.mli`, with a longer content, it will not work as expected:

  $ ocamlc -pp "dune exec -- gospel pps" -dsource -w +50 foo.mli 2>&1 | tail -n 2
  Warning 50 [unexpected-docstring]: unattached documentation comment (ignored)
  val f : int -> int[@@gospel {| y = f x |}]

Finally, just a little clean up after ourselves.

  $ rm foo.mli
