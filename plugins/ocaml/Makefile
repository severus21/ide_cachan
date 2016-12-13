
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
	-rm *.cmx *.cmi
	-rm plugin.cmxs

%.cmi: %.mli
	$(BUILD)

%.cmx: %.ml
	$(BUILD)

depend:
	ocamldep -native *.mli *.ml > .depend

include .depend
