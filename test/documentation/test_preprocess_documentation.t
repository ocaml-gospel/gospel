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
preprocessor. We also enable the compiler warning about unexpected docstring:

  $ ocamlc -pp "dune exec -- gospel pps" -dsource -w +50 foo.mli
  val f : int -> int[@@ocaml.doc " documentation "][@@gospel {| y = f x |}]

Finally, just a little clean up after ourselves.

  $ rm foo.mli
