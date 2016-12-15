DEBUG_OPTION=-g
SOURCE_DIR=src/

PACKAGES=-package lablgtk2 -package oUnit -package compiler-libs.common
LIBS=-lib dynlink
BUILD=ocamlbuild -no-hygiene -r \
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION) -w +A@1..3@5@8..28@30..47-48@49..59" \
	  $(PACKAGES) $(LIBS)

BUILD_DIR=debug/

DOC_DIR=ide.docdir

default: debug

debug:
	@rm -f ide.debug
	$(BUILD) src/ide.native
	@ln -s $(BUILD_DIR)/src/ide.native ide.debug

test:
	@rm -f test.debug
	$(BUILD) tests/test.native
	@ln -s $(BUILD_DIR)/tests/test.native test.debug

runtests: test
	./test.debug -no-cache-filename -output-file test_logs.log

plugins: debug
	cd plugins && make clean && make

release:
	@rm -f ide.release
	$(BUILD) src/ide.native
	@ln -s $(BUILD_DIR)src/ide.native ide.release

doc: debug
	ocamlbuild -use-ocamlfind -no-hygiene $(PACKAGES) $(DOC_DIR)/index.html
	cd plugins && make doc

clean:
	@rm -rf debug/
	@rm -rf release/
	@ocamlbuild -clean

mrproper: clean
	@rm -f *.debug *.release *.dvi *.tex *.log *.pdf *.aux oUnit*
	@rm -rf doc/
