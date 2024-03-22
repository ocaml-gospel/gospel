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

Another problematic interleaving is the documentation of ghost declaration with
and without specifications:

  $ cat > foo.mli << EOF
  > (*@ type casper *)
  > (** a ghost type *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@gospel {| type casper |}]
  [@@@ocaml.text {| a ghost type |}]

  $ cat > foo.mli << EOF
  > (*@ type casper *) (** a ghost type *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@gospel {| type casper |}]
  [@@@ocaml.text {| a ghost type |}]

  $ cat > foo.mli << EOF
  > (*@ type casper *)
  > (*@ model transparent : bool *)
  > (** a ghost type *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@ocaml.text {| a ghost type |}]
  [@@@gospel {| type casper |}[@@gospel {| model transparent : bool |}]]

  $ cat > foo.mli << EOF
  > (*@ type casper *)
  > (** a ghost type *)
  > (*@ model transparent : bool *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@gospel {| type casper |}[@@gospel {| model transparent : bool |}]]
  [@@@ocaml.text {| a ghost type |}]

  $ cat > foo.mli << EOF
  > (** a ghost type *)
  > (*@ type casper *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@ocaml.text {| a ghost type |}]
  [@@@gospel {| type casper |}]

  $ cat > foo.mli << EOF
  > (** a ghost type *)
  > (*@ type casper *)
  > (*@ model transparent : bool *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@ocaml.text {| a ghost type |}]
  [@@@gospel {| type casper |}[@@gospel {| model transparent : bool |}]]

We try to mimick OCaml behaviour regarding comments:

  $ cat > foo.mli << EOF
  > (*@ type casper *)
  > (*
  > (* inner comment *)
  > *)
  > (** a ghost type *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@gospel {| type casper |}]
  [@@@ocaml.text {| a ghost type |}]

We try to mimick OCaml behaviour regarding when a Gospel specification and/or a
documentation should be attached to a value:

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > (* inner comment *)
  > *)
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > (* inner comment *)
  > 
  > *)
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > (* inner comment *)
  > *)
  > 
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > "*)"
  > *)
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > {longstring|
  > 
  > |longstring}
  > *)
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > val x : int
  > (*
  > {longstring|
  > 
  > |longstring}
  > *)
  > 
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@gospel {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > (** [x]'s documentation *)
  > (*
  > {longstring|
  > 
  > |longstring}
  > *)
  > val x : int
  > (*
  > {longstring|
  > 
  > |longstring}
  > *)
  > 
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  val x : int[@@ocaml.doc " [x]'s documentation "][@@gospel
                                                    {| ensures x = 0 |}]

  $ cat > foo.mli << EOF
  > (** Module's documentation *)
  > 
  > val x : int
  > (*
  > {longstring|
  > 
  > |longstring}
  > *)
  > (*@ ensures x = 0 *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  [@@@ocaml.text " Module's documentation "]
  val x : int[@@gospel {| ensures x = 0 |}]

Interleaving ghost-documentation-specification is not accepted if there is more
than one newline between at least two of the consecutive elements:

  $ cat > foo.mli << EOF
  > (*@ type casper *)
  > 
  > (** a ghost type *)
  > (*@ model transparent : bool *)
  > EOF
  $ ocamlc -pp "gospel pps" -dsource -w +50 foo.mli
  File "foo.mli", line 4, characters 0-3:
  4 | [@@gospel
      ^^^
  Error: Syntax error
  [2]


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
  # 1 "foo.mli"
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

Another example about error reporting and location:

  $ cat > foo.mli << EOF
  > (*@ function f (x : integer) : integer *)
  > (** A ghost function
  > 
  > with some documentation *)
  > (*@ ensures x *)
  > EOF
  $ gospel check foo.mli
  File "foo.mli", line 5, characters 12-13:
  5 | (*@ ensures x *)
                  ^
  Error: This term has type integer but a term was expected of type bool.
  [125]

Another corner case is the empty documentation attribute:

  $ cat > foo.mli << EOF
  > val f : int -> int
  > (**)
  > (*@ y = f x z *)
  > EOF
  $ gospel pps foo.mli
  # 1 "foo.mli"
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
  # 1 "foo.mli"
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
  # 1 "foo.mli"
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
  # 1 "foo.mli"
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
