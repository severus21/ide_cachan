DEBUG_OPTION=-g
SOURCE_DIR=src/

BUILD=ocamlbuild \
	  -I src -I tests \
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION) -w +A@1..3@5@8..28@30..47-48@49..59" \
	  -package lablgtk2 -package oUnit -package compiler-libs.common\

BUILD_DIR=debug/

default:
	@rm -f ide.debug
	$(BUILD) src/ide.native
	@ln -s $(BUILD_DIR)/src/ide.native ide.debug

test :
	BUILD_DIR=test/
	TARGET=test
	@rm -f test.debug
	$(BUILD) tests/test.native
	@ln -s $(BUILD_DIR)/tests/test.native test.debug

runtests : test
	./test.debug

release:
	DEBUG_OPTION=
	TARGET=ide
	BUILD_DIR=release/
	@rm -f $(TARGET).release
	$(BUILD)$(SOURCE_DIR)$(TARGET).native

	@ln -s $(BUILD_DIR)$(SOURCE_DIR)ide.native ide.release

clean:
	@rm -rf debug/
	@rm -rf release/
	@rm -f oUnit*
	@ocamlbuild -clean

mrpropre: clean
	@rm -f *.debug *.release *.dvi *.tex *.log *.pdf *.aux
