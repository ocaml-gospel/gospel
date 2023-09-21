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

  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val f : int -> int[@@ocaml.doc {| documentation |}][@@gospel {| y = f x |}]

We now test the locations.

In order to test the locations, we will make an error in the gospel
specifications after an multi-line informal documentation block:

  $ cat > foo.mli << EOF
  > val f : int -> int
  > (** multi
  >     (* line *)
  >     documentation *)
  > (*@ y = f x z *)
  > EOF

Gospel preprocessing should indicate the correct location with the `#` syntax:

  $ gospel pps foo.mli
  val f : int -> int
  [@@ocaml.doc
  # 2 "foo.mli"
   {| multi
      (* line *)
      documentation |}
  # 4 "foo.mli"
                     ]
  [@@gospel
  # 5 "foo.mli"
   {| y = f x z |}
  # 5 "foo.mli"
                 ]
  



Gospel typechecking should spot an error and locate it at the fifth line of the file:

  $ gospel check foo.mli
  File "foo.mli", line 5, characters 8-9:
  5 | (*@ y = f x z *)
              ^
  Error: Type checking error: too many parameters.
  [125]


Another corner case is the empty documentation attibute:

  $ cat > foo.mli << EOF
  > val f : int -> int
  > (**)
  > (*@ y = f x z *)
  > EOF
  $ gospel pps foo.mli
  val f : int -> int
  [@@ocaml.doc
  # 2 "foo.mli"
     ]
  [@@gospel
  # 3 "foo.mli"
   {| y = f x z |}
  # 3 "foo.mli"
                 ]
  



And some other corner cases with different spacing:

  $ cat > foo.mli << EOF
  > val f : int -> int(*@ y = f x z *)
  > EOF
  $ gospel pps foo.mli
  val f : int -> int[@@gospel
  # 1 "foo.mli"
                     {| y = f x z |}
  # 1 "foo.mli"
                                   ]
  



  $ cat > foo.mli << EOF
  > val f : int -> int(** documentation *)
  > (*@ y = f x *)
  > EOF
  $ gospel pps foo.mli
  val f : int -> int[@@ocaml.doc
  # 1 "foo.mli"
                     {| documentation |}
  # 1 "foo.mli"
                                       ]
  [@@gospel
  # 2 "foo.mli"
   {| y = f x |}
  # 2 "foo.mli"
               ]
  



  $ cat > foo.mli << EOF
  > val f : int -> int(** documentation *)(*@ y = f x *)
  > EOF
  $ gospel pps foo.mli
  val f : int -> int[@@ocaml.doc
  # 1 "foo.mli"
                     {| documentation |}
  # 1 "foo.mli"
                                       ][@@gospel
  # 1 "foo.mli"
                                         {| y = f x |}
  # 1 "foo.mli"
                                                     ]
  



Finally, just a little clean up after ourselves.

  $ rm foo.mli
