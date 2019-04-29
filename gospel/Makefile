
export OCAMLFIND_IGNORE_DUPS_IN=$(HOME)/.opam/4.07.0/lib/ocaml/compiler-libs

OCAMLLIBS=$(HOME)/.opam/4.07.0/lib/ocaml/compiler-libs/

OCAMLBUILD=ocamlbuild  -I ocaml-lib -I tools -use-ocamlfind -pkg compiler-libs.common \
-use-menhir -menhir "menhir -v --explain"

gospel: ocaml-lib/*.ml ocaml-lib/*.mli ocaml-lib/*.mly ocaml-lib/*.mll tools/*.ml
	$(OCAMLBUILD) gospel.native

dependency_graph: gospel
	depgraph _build/src/ > dependencies.dot
#	use xdot for instance to see dependencies.dot

doc: ocaml-lib/*.ml ocaml-lib/*.mli ocaml-lib/*.mly tools/*.ml
	$(OCAMLBUILD) doc.docdir/index.html

clean:
	ocamlbuild -clean
