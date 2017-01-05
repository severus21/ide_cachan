
CMIS=$(subst .mli,.cmi,$(wildcard *.mli))
CMXS=$(subst .ml,.cmx,$(shell ocamldep -native -sort *.ml))

INCLUDES=-I ../../debug/src/ -I ../../debug/src/plugins/

PACKAGES=-package compiler-libs.common -package oUnit

BUILD=ocamlfind ocamlopt $(PACKAGES) \
	  $(INCLUDES) -g dynlink.cmxa unix.cmxa -c $< -o $@

plugin.cmxs: $(CMIS) $(CMXS)
	ocamlfind ocamlopt $(PACKAGES) -linkpkg $(INCLUDES) -g -shared \
		dynlink.cmxa core.cmx extract.cmx factory.cmx \
		$(CMXS) -o plugin.cmxs

default: plugin.cmxs

all: depend

clean:
	@rm -f *.cmx *.cmi *.o
	@rm -f plugin.cmxs

mrproper: clean
	@rm -rf doc

.PHONY: doc

doc: plugin.cmxs
	@rm -rf doc
	@mkdir doc
	ocamlfind ocamldoc $(PACKAGES) $(INCLUDES) -html -d doc/ *.mli

%.cmi: %.mli
	$(BUILD)

%.cmx: %.ml
	$(BUILD)

depend: .depend

.depend:
	ocamldep -native *.mli *.ml > .depend

include .depend
