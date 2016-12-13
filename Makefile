DEBUG_OPTION=-g
SOURCE_DIR=src/

PACKAGES=-package lablgtk2 -package oUnit -package compiler-libs.common
LIBS=-lib dynlink
BUILD=ocamlbuild -r \
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION) -w +A@1..3@5@8..28@30..47-48@49..59" \
	  $(PACKAGES) $(LIBS)

BUILD_DIR=debug/

DOC_DIR=ide.docdir

default: debug

debug: clean
	@rm -f ide.debug
	$(BUILD) src/ide.native
	@ln -s $(BUILD_DIR)/src/ide.native ide.debug

test: clean
	@rm -f test.debug
	$(BUILD) tests/test.native
	@ln -s $(BUILD_DIR)/tests/test.native test.debug

runtests: test
	./test.debug -no-cache-filename -output-file test_logs.log

plugins: clean debug
	cd plugins && make clean && make

release: clean
	@rm -f ide.release
	$(BUILD) src/ide.native
	@ln -s $(BUILD_DIR)src/ide.native ide.release

doc: clean
	ocamlbuild -use-ocamlfind $(PACKAGES) $(DOC_DIR)/index.html
	make debug
	cd plugins && make doc
	make clean

clean:
	@rm -rf debug/
	@rm -rf release/
	@ocamlbuild -clean

mrproper: clean
	@rm -f *.debug *.release *.dvi *.tex *.log *.pdf *.aux oUnit*
	@rm -rf doc/
