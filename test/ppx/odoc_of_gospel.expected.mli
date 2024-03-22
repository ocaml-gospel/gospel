[@@@ocaml.text " Module informal documentation "]
[@@@ocaml.text " An axiom declaration "]
[@@@gospel {| axiom a : true |}]
[@@@ocaml.text "{@gospel[\nGospel declaration:\n    axiom a : true ]}"]
[@@@ocaml.text " A logical function declaration without definition "]
[@@@gospel {| function f : integer -> integer |}]
[@@@ocaml.text
  "{@gospel[\nGospel declaration:\n    function f : integer -> integer ]}"]
[@@@ocaml.text " A logical function definition "]
[@@@gospel {| function g (i : integer) : integer = i + 1 |}]
[@@@ocaml.text
  "{@gospel[\nGospel declaration:\n    function g (i : integer) : integer = i + 1 ]}"]
[@@@ocaml.text " A logical function declaration with assertions "]
[@@@gospel
  {| function h (i : integer) : integer = i - 1 |}[@@gospel
                                                    {| requires i > 0
    ensures result >= 0 |}]
  [@@ocaml.doc
    "{@gospel[\nGospel specification:\n    requires i > 0\n    ensures result >= 0 ]}"]]
[@@@ocaml.text
  "{@gospel[\nGospel declaration:\n    function h (i : integer) : integer = i - 1 \n    requires i > 0\n    ensures result >= 0 ]}"]
[@@@ocaml.text " A logical predicate definition "]
[@@@gospel {| predicate p (i : integer) = i = 42 |}]
[@@@ocaml.text
  "{@gospel[\nGospel declaration:\n    predicate p (i : integer) = i = 42 ]}"]
[@@@ocaml.text " A ghost type declaration "]
[@@@gospel {| type casper |}]
[@@@ocaml.text "{@gospel[\nGospel declaration:\n    type casper ]}"]
type 'a t[@@ocaml.doc {| A program type declaration with specifications |}]
[@@gospel {| model m : 'a sequence
    with x
    invariant true |}][@@ocaml.doc
                                                                    "{@gospel[\nGospel specification:\n    model m : 'a sequence\n    with x\n    invariant true ]}"]
val prog_fun : int -> int[@@ocaml.doc
                           {| A program function with specifications |}]
[@@gospel {| y = prog_fun x
    requires true
    ensures true |}][@@ocaml.doc
                                                                    "{@gospel[\nGospel specification:\n    y = prog_fun x\n    requires true\n    ensures true ]}"]
val multiple_gospel_attribute : int -> int[@@gospel
                                            {| y = multiple_gospel_attribute x |}]
[@@gospel {| requires true |}][@@gospel {| ensures true |}][@@ocaml.doc
                                                             "{@gospel[\nGospel specification:\n    y = multiple_gospel_attribute x ]}"]
[@@ocaml.doc "{@gospel[\nGospel specification:\n    requires true ]}"]
[@@ocaml.doc "{@gospel[\nGospel specification:\n    ensures true ]}"]
