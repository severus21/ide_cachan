DEBUG_OPTION=-g
SOURCE_DIR=src/
BUILD_DIR=debug/
BUILD=ocamlbuild \
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION) -w +A@1..3@5@8..28@30..47@49..59" \
	  -package lablgtk2 -package oUnit -package compiler-libs.common\
	  $(SOURCE_DIR)ide.native

default:
	-rm ide.debug
	$(BUILD)
	ln -s $(BUILD_DIR)$(SOURCE_DIR)ide.native ide.debug
release:
	DEBUG_OPTION=
	BUILD_DIR=release/
	-rm ide.release
	$(BUILD)
	ln -s $(BUILD_DIR)$(SOURCE_DIR)ide.native ide.release

clean:
	-rm -rf debug/
	-rm -rf release/
	-rm oUnit*
	ocamlbuild -clean
