In this file, we are testing output from Odoc after the ppx rewriting.

First, we compile the file, running the source preprocessor and the ppx:

  $ ocamlc -bin-annot -pp "gospel pps" -ppx "./pp.exe -as-ppx" odoc_of_gospel.mli

Then run Odoc

  $ odoc compile odoc_of_gospel.cmti
  File "odoc_of_gospel.mli", line 5, character 6 to line 7, character 21:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 9, character 6 to line 11, character 38:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 13, character 6 to line 15, character 49:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 17, character 6 to line 21, character 26:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 23, character 6 to line 25, character 41:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 27, character 6 to line 29, character 18:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 31, character 6 to line 35, character 21:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 37, character 6 to line 41, character 19:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 42, character 6 to line 44, character 38:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 43, character 6 to line 45, character 20:
  Warning: Code blocks should be indented at the opening `{`.
  File "odoc_of_gospel.mli", line 44, character 6 to line 46, character 19:
  Warning: Code blocks should be indented at the opening `{`.

We test the html and the latex outputs

  $ odoc html-generate odoc_of_gospel.odoc -o tmp
  $ odoc latex-generate odoc_of_gospel.odoc -o tmp

As the output is not stable through odoc versions, we just check that the files
are generated:

  $ find tmp -name \*.tex
  tmp/Odoc_of_gospel.tex
  $ find tmp -name \*.html
  tmp/Odoc_of_gospel/index.html

This is not the case for the man output, here we can test the output.
Though, to be sure the diff is meaningful, let's set terminal's number of
columns:

  $ export COLUMNS=80
  $ odoc man-generate odoc_of_gospel.odoc -o tmp
  $ grep -v '^\.' tmp/Odoc_of_gospel.3o
  
  Odoc_of_gospel
  \fBModule Odoc_of_gospel\fR
  Module informal documentation
  An axiom declaration
  Gospel declaration:
      axiom a : true 
  A logical function declaration without definition
  Gospel declaration:
      function f : integer -> integer 
  A logical function definition
  Gospel declaration:
      function g (i : integer) : integer = i + 1 
  A logical function declaration with assertions
  Gospel declaration:
      function h (i : integer) : integer = i - 1 
      requires i > 0
      ensures result >= 0 
  A logical predicate definition
  Gospel declaration:
      predicate p (i : integer) = i = 42 
  A ghost type declaration
  Gospel declaration:
      type casper 
  \f[CB]type\fR 'a t
  A program type declaration with specifications
  Gospel specification:
      model m : 'a sequence
      with x
      invariant true 
  
  \f[CB]val\fR prog_fun : int \f[CB]\->\fR int
  A program function with specifications
  Gospel specification:
      y = prog_fun x
      requires true
      ensures true 
  
  \f[CB]val\fR multiple_gospel_attribute : int \f[CB]\->\fR int
  Gospel specification:
      y = multiple_gospel_attribute x 
  Gospel specification:
      requires true 
  Gospel specification:
      ensures true 
  
  
