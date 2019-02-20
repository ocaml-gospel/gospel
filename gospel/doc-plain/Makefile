
OCAMLC    = ocamlc.opt
OCAMLLEX  = ocamllex.opt
RUBBER    = rubber
MKDIR_P   = mkdir -p

REFMANUAL = ref-manual

BNF = ident qualid operator type attribute term1 term2 term3 \
			spec val-spec type-spec logic-spec ghost-spec
BNFTEX = $(BNF:%=generated/%_bnf.tex)


doc: $(BNFTEX) $(REFMANUAL).tex
	rubber --warn all --pdf $(REFMANUAL).tex

generated/%_bnf.tex: %.bnf bnf
	$(MKDIR_P) generated
	./bnf $< > $@

bnf: bnf.mll
	$(OCAMLLEX)	$<
	$(OCAMLC) -o $@ bnf.ml

clean:
	rm -rf generated 
	rm -rf bnf bnf.cmo bnf.cmi bnf.ml
	rm -rf $(REFMANUAL).pdf $(REFMANUAL).log $(REFMANUAL).aux \
	       $(REFMANUAL).out $(REFMANUAL).ind $(REFMANUAL).idx \
				 $(REFMANUAL).ilg $(REFMANUAL).toc
