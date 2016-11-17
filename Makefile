DEBUG_OPTION=-g
SOURCE_DIR=src/

BUILD=ocamlbuild \
	  -I structure\
	  -I tests\
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION) -w +A@1..3@5@8..28@30..47@49..59" \
	  -package lablgtk2 -package oUnit -package compiler-libs.common\
	  $(SOURCE_DIR)

BUILD_DIR=debug/
TARGET=ide

default:
	-rm -f $(TARGET).debug
	$(BUILD)$(TARGET).native
	ln -s $(BUILD_DIR)$(SOURCE_DIR)$(TARGET).native $(TARGET).debug
test : 
	BUILD_DIR=test/
	TARGET=test
	-rm -f test.debug
	$(BUILD)test.native
	ln -s $(BUILD_DIR)$(SOURCE_DIR)test.native test.debug

release:
	DEBUG_OPTION=
	TARGET=ide
	BUILD_DIR=release/
	-rm -f $(TARGET).release
	$(BUILD)$(TARGET).native

	ln -s $(BUILD_DIR)$(SOURCE_DIR)ide.native ide.release

clean:
	-rm -rf debug/
	-rm -rf release/
	-rm oUnit*
	ocamlbuild -clean
